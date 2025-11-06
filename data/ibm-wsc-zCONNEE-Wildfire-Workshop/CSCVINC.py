"""
Python Translation of CSCVINC.cbl

Program: CSCVINC
Type: CICS Program
Function: File operations (FILEA) via CICS channels and containers

This program:
1. Reads request from CICS channel container
2. Performs file operation (DELETE, INSERT, UPDATE, READ) based on ACTION
3. Writes response to channel container
4. Writes messages to CSMT transient data queue
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Dict, List, Tuple
from datetime import datetime


# ============================================================================
# Constants
# ============================================================================

DFHRESP_NORMAL = 0
DFHRESP_CONTAINERERR = 3
DFHRESP_CCSIDERR = 3  # Same as CONTAINERERR but different context
DFHRESP_NOTFND = 12
DFHRESP_DUPKEY = 22

FILEA_NAME = "FILEA"
CSMT_QUEUE = "CSMT"


# ============================================================================
# FileA Record Structure
# ============================================================================

@dataclass
class FileAArea:
    """FileA record area structure."""
    stat: str = ""  # 1 char
    numb: str = ""  # 6 chars (key)
    name: str = ""  # 20 chars
    addrx: str = ""  # 20 chars
    phone: str = ""  # 8 chars
    datex: str = ""  # 8 chars
    amount: str = ""  # 8 chars
    comment: str = ""  # 9 chars
    
    def to_string(self) -> str:
        """Convert to fixed-width string (80 chars total)."""
        return (
            f"{self.stat:1s}"
            f"{self.numb:6s}"
            f"{self.name:20s}"
            f"{self.addrx:20s}"
            f"{self.phone:8s}"
            f"{self.datex:8s}"
            f"{self.amount:8s}"
            f"{self.comment:9s}"
        )[:80].ljust(80)
    
    @classmethod
    def from_string(cls, record_str: str) -> 'FileAArea':
        """Create from fixed-width string."""
        record_str = record_str.ljust(80)
        return cls(
            stat=record_str[0:1].strip(),
            numb=record_str[1:7].strip(),
            name=record_str[7:27].strip(),
            addrx=record_str[27:47].strip(),
            phone=record_str[47:55].strip(),
            datex=record_str[55:63].strip(),
            amount=record_str[63:71].strip(),
            comment=record_str[71:80].strip()
        )


# ============================================================================
# Request Container Structure (CSCCREQ)
# ============================================================================

@dataclass
class RequestContainer:
    """Request container structure (CSCCREQ)."""
    action: str = ""  # Action code: 'D' (DELETE), 'I' (INSERT), 'U' (UPDATE), other (READ)
    numb: str = ""  # Number (6 chars)
    filea_area: FileAArea = field(default_factory=FileAArea)


# ============================================================================
# Response Container Structure (CSCCRESP)
# ============================================================================

@dataclass
class ResponseContainer:
    """Response container structure (CSCCRESP)."""
    userid: str = ""  # User ID (8 chars)
    action: str = ""  # Action code
    numb: str = ""  # Number (6 chars)
    filea_area: FileAArea = field(default_factory=FileAArea)


# ============================================================================
# CICS Channel/Container Simulation
# ============================================================================

class CICSChannel:
    """Simulates CICS channel with containers."""
    
    def __init__(self):
        self._containers: Dict[str, bytes] = {}
        self._browse_tokens: Dict[int, List[str]] = {}
        self._next_token: int = 1
    
    def startbrowse_container(self, channel_name: str) -> Tuple[int, int, int]:
        """Start browsing containers in channel (CICS STARTBROWSE CONTAINER).
        
        Args:
            channel_name: Channel name
        
        Returns:
            Tuple of (token, resp_code, resp2_code)
        """
        # Find all containers for this channel
        container_names = []
        for key in self._containers.keys():
            if key.startswith(f"{channel_name}:"):
                container_name = key.split(":", 1)[1]
                container_names.append(container_name)
        
        if not container_names:
            # Channel doesn't exist or has no containers - return error
            return 0, 3, 0
        
        # Create browse token
        token = self._next_token
        self._next_token += 1
        self._browse_tokens[token] = container_names.copy()
        
        return token, DFHRESP_NORMAL, 0
    
    def getnext_container(self, token: int) -> Tuple[Optional[str], int, int]:
        """Get next container name (CICS GETNEXT CONTAINER).
        
        Args:
            token: Browse token
        
        Returns:
            Tuple of (container_name, resp_code, resp2_code)
        """
        if token not in self._browse_tokens:
            return None, 3, 0
        
        containers = self._browse_tokens[token]
        if not containers:
            return None, 12, 0  # NOTFND
        
        container_name = containers.pop(0)
        return container_name, DFHRESP_NORMAL, 0
    
    def get_container_flength(self, channel_name: str, container_name: str) -> Tuple[int, int, int]:
        """Get container length (CICS GET CONTAINER with NODATA).
        
        Args:
            channel_name: Channel name
            container_name: Container name
        
        Returns:
            Tuple of (length, resp_code, resp2_code)
        """
        channel_key = f"{channel_name}:{container_name}"
        if channel_key not in self._containers:
            return 0, DFHRESP_CONTAINERERR, 0
        
        length = len(self._containers[channel_key])
        return length, DFHRESP_NORMAL, 0
    
    def get_container(self, channel_name: str, container_name: str) -> Tuple[Optional[bytes], int, int]:
        """Get container data (CICS GET CONTAINER).
        
        Args:
            channel_name: Channel name
            container_name: Container name
        
        Returns:
            Tuple of (data, resp_code, resp2_code)
        """
        channel_key = f"{channel_name}:{container_name}"
        if channel_key not in self._containers:
            return None, DFHRESP_CONTAINERERR, 0
        
        return self._containers[channel_key], DFHRESP_NORMAL, 0
    
    def put_container(self, channel_name: str, container_name: str, data: bytes) -> Tuple[int, int]:
        """Put container data (CICS PUT CONTAINER).
        
        Args:
            channel_name: Channel name
            container_name: Container name
            data: Container data
        
        Returns:
            Tuple of (resp_code, resp2_code)
        """
        channel_key = f"{channel_name}:{container_name}"
        self._containers[channel_key] = data
        return DFHRESP_NORMAL, 0
    
    def assign_channel(self) -> str:
        """Assign channel name (CICS ASSIGN CHANNEL).
        
        Returns:
            Channel name (empty if no channel)
        """
        # Return first channel found or empty
        channels = set()
        for key in self._containers.keys():
            channel = key.split(":")[0]
            channels.add(channel)
        
        if channels:
            return list(channels)[0]
        return ""


# ============================================================================
# CICS File Handler (FILEA)
# ============================================================================

class CICSFileHandler:
    """Handler for CICS FILEA operations."""
    
    def __init__(self, filename: str = FILEA_NAME):
        self.filename = filename
        self._data: Dict[str, FileAArea] = {}  # Key: numb
        self._locked_records: Dict[str, FileAArea] = {}  # For UPDATE operations
    
    def read_file(self, numb: str, update: bool = False) -> Tuple[Optional[FileAArea], int, int]:
        """Read file record (CICS READ FILE).
        
        Args:
            numb: Record key
            update: If True, lock for update (CICS READ UPDATE)
        
        Returns:
            Tuple of (record, resp_code, resp2_code)
        """
        numb = numb.strip()
        if numb in self._data:
            record = self._data[numb]
            if update:
                # Lock record for update
                self._locked_records[numb] = record
            return record, DFHRESP_NORMAL, 0
        else:
            return None, DFHRESP_NOTFND, 0
    
    def write_file(self, record: FileAArea) -> Tuple[int, int]:
        """Write file record (CICS WRITE FILE).
        
        Args:
            record: FileA record to write
        
        Returns:
            Tuple of (resp_code, resp2_code)
        """
        key = record.numb.strip()
        if not key:
            return 3, 0  # CONTAINERERR
        
        if key in self._data:
            return DFHRESP_DUPKEY, 0
        
        self._data[key] = record
        return DFHRESP_NORMAL, 0
    
    def rewrite_file(self, record: FileAArea) -> Tuple[int, int]:
        """Rewrite file record (CICS REWRITE FILE).
        
        Args:
            record: FileA record to rewrite
        
        Returns:
            Tuple of (resp_code, resp2_code)
        """
        key = record.numb.strip()
        if not key:
            return 3, 0
        
        if key not in self._locked_records:
            return DFHRESP_NOTFND, 0
        
        self._data[key] = record
        del self._locked_records[key]
        return DFHRESP_NORMAL, 0
    
    def delete_file(self, numb: str) -> Tuple[int, int]:
        """Delete file record (CICS DELETE FILE).
        
        Args:
            numb: Record key
        
        Returns:
            Tuple of (resp_code, resp2_code)
        """
        numb = numb.strip()
        if numb not in self._data:
            return DFHRESP_NOTFND, 0
        
        del self._data[numb]
        if numb in self._locked_records:
            del self._locked_records[numb]
        return DFHRESP_NORMAL, 0
    
    def add_record(self, record: FileAArea):
        """Add record (for testing)."""
        if record.numb:
            self._data[record.numb.strip()] = record
    
    def clear_records(self):
        """Clear all records (for testing)."""
        self._data = {}
        self._locked_records = {}


# ============================================================================
# CSMT Queue Handler
# ============================================================================

class CSMTQueueHandler:
    """Handler for CSMT transient data queue."""
    
    def __init__(self):
        self._queue: List[str] = []
    
    def writeq_td(self, data: str) -> int:
        """Write to TD queue (CICS WRITEQ TD).
        
        Args:
            data: Data to write
        
        Returns:
            Response code (0 = success)
        """
        self._queue.append(data)
        return 0
    
    def get_messages(self) -> List[str]:
        """Get all messages (for testing)."""
        return self._queue.copy()
    
    def clear_queue(self):
        """Clear queue (for testing)."""
        self._queue = []


# ============================================================================
# Main Program
# ============================================================================

def process_filea_operation(
    action: str,
    request_container: RequestContainer,
    file_handler: CICSFileHandler
) -> Tuple[FileAArea, int, int, str]:
    """Process FILEA operation based on action.
    
    Args:
        action: Action code ('D', 'I', 'U', or other for READ)
        request_container: Request container
        file_handler: FILEA file handler
    
    Returns:
        Tuple of (filea_area, resp_code, resp2_code, error_message)
    """
    error_msg = ""
    resp_code = DFHRESP_NORMAL
    resp2_code = 0
    filea_area = FileAArea()
    
    if action == 'D':  # DELETE
        resp_code, resp2_code = file_handler.delete_file(request_container.numb)
        if resp_code != DFHRESP_NORMAL:
            error_msg = "Error deleting record "
        else:
            filea_area = request_container.filea_area
    
    elif action == 'I':  # INSERT/WRITE
        resp_code, resp2_code = file_handler.write_file(request_container.filea_area)
        if resp_code != DFHRESP_NORMAL:
            error_msg = "Error writing record "
        else:
            filea_area = request_container.filea_area
    
    elif action == 'U':  # UPDATE
        # Read for update
        record, read_resp, read_resp2 = file_handler.read_file(request_container.numb, update=True)
        if read_resp != DFHRESP_NORMAL:
            error_msg = "Error reading a record for update "
            resp_code = read_resp
            resp2_code = read_resp2
        else:
            # Rewrite
            resp_code, resp2_code = file_handler.rewrite_file(request_container.filea_area)
            if resp_code != DFHRESP_NORMAL:
                error_msg = "Error rewriting record "
            else:
                filea_area = request_container.filea_area
    
    else:  # READ
        record, read_resp, read_resp2 = file_handler.read_file(request_container.numb)
        if read_resp != DFHRESP_NORMAL:
            error_msg = "Error reading record "
            resp_code = read_resp
            resp2_code = read_resp2
        else:
            filea_area = record if record else FileAArea()
    
    return filea_area, resp_code, resp2_code, error_msg


def get_and_format_current_time() -> Tuple[str, str]:
    """Get and format current time (Get-and-Format-Current-Time).
    
    Returns:
        Tuple of (current_date, current_time)
    """
    now = datetime.now()
    current_date = now.strftime("%Y/%m/%d")
    current_time = now.strftime("%H:%M:%S")
    return current_date, current_time


def write_to_csmt_queue(message: str, csmt_handler: CSMTQueueHandler) -> int:
    """Write message to CSMT queue (Write-to-CSMT-Queue).
    
    Args:
        message: Message to write
        csmt_handler: CSMT queue handler
    
    Returns:
        Response code
    """
    current_date, current_time = get_and_format_current_time()
    output_area = f"CSCVINC {current_date} {current_time} {message}"
    return csmt_handler.writeq_td(output_area)


def serialize_request_container(request: RequestContainer) -> bytes:
    """Serialize request container to bytes."""
    # Simple serialization: action(1) + numb(6) + filea_area(80)
    action_bytes = request.action.encode('utf-8')[:1].ljust(1, b' ')
    numb_bytes = request.numb.encode('utf-8')[:6].ljust(6, b' ')
    filea_bytes = request.filea_area.to_string().encode('utf-8')
    return action_bytes + numb_bytes + filea_bytes


def deserialize_request_container(data: bytes) -> RequestContainer:
    """Deserialize bytes to request container."""
    request = RequestContainer()
    if len(data) >= 1:
        request.action = data[0:1].decode('utf-8').strip()
    if len(data) >= 7:
        request.numb = data[1:7].decode('utf-8').strip()
    if len(data) >= 87:
        filea_str = data[7:87].decode('utf-8')
        request.filea_area = FileAArea.from_string(filea_str)
    return request


def serialize_response_container(response: ResponseContainer) -> bytes:
    """Serialize response container to bytes."""
    # Simple serialization: userid(8) + action(1) + numb(6) + filea_area(80)
    userid_bytes = response.userid.encode('utf-8')[:8].ljust(8, b' ')
    action_bytes = response.action.encode('utf-8')[:1].ljust(1, b' ')
    numb_bytes = response.numb.encode('utf-8')[:6].ljust(6, b' ')
    filea_bytes = response.filea_area.to_string().encode('utf-8')
    return userid_bytes + action_bytes + numb_bytes + filea_bytes


def deserialize_response_container(data: bytes) -> ResponseContainer:
    """Deserialize bytes to response container."""
    response = ResponseContainer()
    if len(data) >= 8:
        response.userid = data[0:8].decode('utf-8').strip()
    if len(data) >= 9:
        response.action = data[8:9].decode('utf-8').strip()
    if len(data) >= 15:
        response.numb = data[9:15].decode('utf-8').strip()
    if len(data) >= 95:
        filea_str = data[15:95].decode('utf-8')
        response.filea_area = FileAArea.from_string(filea_str)
    return response


def main(
    channel_name: Optional[str] = None,
    cics_channel: Optional[CICSChannel] = None,
    file_handler: Optional[CICSFileHandler] = None,
    csmt_handler: Optional[CSMTQueueHandler] = None,
    userid: str = "SYSTEM"
) -> Tuple[int, str]:
    """Main program entry point (MAIN-PROCESSING).
    
    Args:
        channel_name: Channel name (if None, will be assigned)
        cics_channel: CICS channel handler (created if None)
        file_handler: FILEA file handler (created if None)
        csmt_handler: CSMT queue handler (created if None)
        userid: CICS user ID
    
    Returns:
        Tuple of (exit_code, error_message)
    """
    # Initialize handlers
    if cics_channel is None:
        cics_channel = CICSChannel()
    if file_handler is None:
        file_handler = CICSFileHandler()
    if csmt_handler is None:
        csmt_handler = CSMTQueueHandler()
    
    # Initialize containers
    request_container = RequestContainer()
    response_container = ResponseContainer()
    response_container.userid = userid
    
    # Assign channel if not provided
    if channel_name is None:
        channel_name = cics_channel.assign_channel()
    
    # Check if channel exists
    if not channel_name or channel_name.strip() == "":
        # Abend with NOCN
        return 1, "No channel provided"
    
    # Start browsing containers
    token, resp, resp2 = cics_channel.startbrowse_container(channel_name)
    if resp != DFHRESP_NORMAL:
        message = "Error invoking CICS STARTBROWSE command "
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    
    # Get next container
    container_name, resp, resp2 = cics_channel.getnext_container(token)
    if resp != DFHRESP_NORMAL:
        message = "Error invoking EXEC GETNEXT CONTAINER "
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    
    # Get container length
    input_length, resp, resp2 = cics_channel.get_container_flength(channel_name, container_name)
    
    # Check response code
    if resp == DFHRESP_CONTAINERERR:
        message = f"{container_name} container was not passed to the program"
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    elif resp == DFHRESP_CCSIDERR:
        if resp == 3:
            message = f"Container {container_name} type is BIT, not CHAR"
        else:
            message = "The GET CONTAINER command returned an unexpected CCSIDERR condition"
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    elif resp != DFHRESP_NORMAL:
        message = "The GET CONTAINER command returned an unexpected response code"
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    
    # Success message (commented out in COBOL)
    # message = f"CSCVINC Read from {container_name} container successfully"
    # write_to_csmt_queue(message, csmt_handler)
    
    # Read container data
    container_data, resp, resp2 = cics_channel.get_container(channel_name, container_name)
    if resp != DFHRESP_NORMAL:
        message = "Error accessing container data"
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    
    # Deserialize request container
    request_container = deserialize_request_container(container_data)
    
    # Copy NUMB and ACTION to response container
    response_container.numb = request_container.numb
    response_container.action = request_container.action
    
    # Process FILEA operation
    filea_area, resp_code, resp2_code, error_msg = process_filea_operation(
        request_container.action,
        request_container,
        file_handler
    )
    
    response_container.filea_area = filea_area
    
    if error_msg:
        write_to_csmt_queue(error_msg, csmt_handler)
    
    # Put response container
    response_data = serialize_response_container(response_container)
    resp, resp2 = cics_channel.put_container(channel_name, container_name, response_data)
    if resp != DFHRESP_NORMAL:
        message = "Error writing reponse container "
        write_to_csmt_queue(message, csmt_handler)
        return 1, message
    
    return 0, ""


def process_request(
    action: str,
    request_container: RequestContainer,
    file_handler: CICSFileHandler,
    csmt_handler: CSMTQueueHandler
) -> Tuple[ResponseContainer, int, str]:
    """Process request and return response.
    
    Args:
        action: Action code
        request_container: Request container
        file_handler: FILEA file handler
        csmt_handler: CSMT queue handler
    
    Returns:
        Tuple of (response_container, exit_code, error_message)
    """
    response_container = ResponseContainer()
    response_container.userid = "SYSTEM"  # Would be from CICS ASSIGN USERID
    response_container.action = action
    response_container.numb = request_container.numb
    
    # Process file operation
    filea_area, resp_code, resp2_code, error_msg = process_filea_operation(
        action,
        request_container,
        file_handler
    )
    
    response_container.filea_area = filea_area
    
    if error_msg:
        write_to_csmt_queue(error_msg, csmt_handler)
    
    exit_code = 0 if resp_code == DFHRESP_NORMAL else 1
    return response_container, exit_code, error_msg

