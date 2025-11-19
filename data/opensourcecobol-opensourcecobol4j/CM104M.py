"""
CM104M - Dual Queue Communication Test Program
Translated from COBOL to Python

Original COBOL Program:
FEDERAL COMPILER TESTING CENTER
GENERAL SERVICES ADMINISTRATION
AUTOMATED DATA AND TELECOMMUNICATION SERVICE.
SOFTWARE DEVELOPMENT OFFICE.
"""

import sys
import time
from datetime import datetime
from typing import Optional


class CommunicationQueue:
    """Simulates COBOL communication queue for input"""
    
    def __init__(self, queue_name: str = ""):
        self.queue_name = queue_name
        self.time_received = None
        self.source = ""
        self.in_length = 0
        self.end_key = ""
        self.in_status = "00"
        self.msg_count = 0
        self.enabled = False
    
    def enable_input(self, key: str):
        """Enable input queue"""
        self.queue_name = key
        self.enabled = True
    
    def receive_message(self) -> Optional[str]:
        """Receive message from queue (simulated via stdin)"""
        if not self.enabled:
            raise RuntimeError("Queue not enabled")
        
        try:
            # Check if there's input available (non-blocking check)
            # Use try/except instead of select for cross-platform compatibility
            try:
                # Try to read without blocking
                import select
                if sys.platform != "win32":
                    if not select.select([sys.stdin], [], [], 0)[0]:
                        return None  # No data available
                # On Windows, select doesn't work with stdin, so we'll try input() directly
                message = input().strip()
            except (EOFError, ValueError, OSError):
                # No data available or input not ready
                return None
            
            self.time_received = datetime.now()
            self.in_length = len(message)
            self.source = f"SOURCE-{self.queue_name[:8]}"  # Simulate source
            self.in_status = "00"
            self.msg_count += 1
            return message
        except Exception:
            self.in_status = "99"
            return None
    
    def get_count(self) -> int:
        """Get message count from queue"""
        return self.msg_count


class OutputQueue:
    """Simulates COBOL communication queue for output"""
    
    def __init__(self, queue_name: str = ""):
        self.queue_name = queue_name
        self.dest_count = 1
        self.out_length = 0
        self.out_status = "00"
        self.err_key = ""
        self.sym_dest = "**** X-CARD UNDEFINED ****"
        self.enabled = False
    
    def enable_output(self, key: str):
        """Enable output queue"""
        self.sym_dest = key
        self.enabled = True
    
    def send_message(self, message: str, emi: bool = False) -> bool:
        """Send message to queue (simulated via stdout)"""
        if not self.enabled:
            raise RuntimeError("Queue not enabled")
        
        try:
            print(message[:self.out_length], end='', flush=True)
            if emi:
                print()  # End message indicator
            self.out_status = "00"
            return True
        except Exception:
            self.out_status = "99"
            self.err_key = "E"
            return False


class CM104M:
    """Main program class"""
    
    def __init__(self):
        # Counters
        self.rec_skl_sub = 0
        self.rec_ct = 0
        self.delete_cnt = 0
        self.error_counter = 0
        self.inspect_counter = 0
        self.pass_counter = 0
        self.total_error = 0
        self.error_hold = 0
        self.dummy_hold = " " * 120
        self.record_count = 0
        
        # Headers
        self.ccvs_h_1 = (" " * 27 + 
                        " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION SYSTEM" + 
                        " " * 26)
        self.ccvs_h_2 = ("CCVS74 NCC  COPY, NOT FOR DISTRIBUTION." + 
                        "TEST RESULTS SET-  " + 
                        " " * 9 + 
                        " " * 40)
        self.ccvs_h_3 = (" FOR OFFICIAL USE ONLY    " + 
                        "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      " + 
                        "  COPYRIGHT   1974 ")
        self.ccvs_e_1 = (" " * 52 + 
                        "END OF TEST-  " + 
                        " " * 9 + 
                        " NTIS DISTRIBUTION COBOL 74")
        self.ccvs_e_2 = (" " * 31 + 
                        " " * 21 + 
                        "   " + 
                        " " + 
                        "ERRORS ENCOUNTERED")
        self.ccvs_e_3 = (" FOR OFFICIAL USE ONLY" + 
                        " " * 12 + 
                        "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     " + 
                        " " * 13 + 
                        " COPYRIGHT 1974")
        self.ccvs_e_4 = ("   " + 
                        " OF " + 
                        "   " + 
                        "  TESTS WERE EXECUTED SUCCESSFULLY")
        
        self.hyphen_line = (" " + 
                           "*" * 65 + 
                           "*" * 54)
        
        self.ccvs_pgm_id = "CM104M"
        self.test_id = ""
        self.id_again = ""
        
        # Log headers
        self.log_hdr_1 = " " * 54 + "MESSAGE LOG"
        self.log_hdr_2 = ("   " + 
                         "SYMBOLIC" + 
                         "TIME MCS" + 
                         "SEND" + 
                         "MSG" + 
                         "IN" + 
                         "OUT")
        self.log_hdr_3 = ("    " + 
                         "SOURCE" + 
                         "RECEIVED" + 
                         "QD" + 
                         "COMPLT" + 
                         "LTH" + 
                         "ST" + 
                         " " * 33 + 
                         "STAT" + 
                         "MESSAGE CONTENTS")
        self.log_hdr_4 = (" " + 
                         "-" * 12 + 
                         " " + 
                         "-" * 11 + 
                         " --" + 
                         "-" * 6 + 
                         " " + 
                         "---" + 
                         "--" + 
                         "----" + 
                         "-" * 72)
        
        # Message structures
        self.msg = " " * 72
        self.kill_field = ""
        
        # Log line components
        self.sym_source = " " * 12
        self.log_time = {"hrs": 0, "mins": 0, "secs": 0.0}
        self.queue_depth = 0
        self.out_time = 0.0
        self.msg_length = 0
        self.in_status = "00"
        self.out_status = "00"
        self.out_err_key = ""
        
        # Time structures
        self.send_time = {"hrs": 0, "mins": 0, "secs": 0.0}
        
        # File handle
        self.print_file = None
        
        # Communication queues - two input and two output
        self.cm_inque_1 = CommunicationQueue("CM-INQUE-1")
        self.cm_inque_2 = CommunicationQueue("CM-INQUE-2")
        self.cm_outque_1 = OutputQueue("CM-OUTQUE-1")
        self.cm_outque_2 = OutputQueue("CM-OUTQUE-2")
        
        # Queue 1 specifications
        self.queue_1 = "**** X-CARD UNDEFINED ****"
        self.time_received_1 = {"hrs": 0, "mins": 0, "secs": 0.0}
        self.source_1 = ""
        self.in_length_1 = 0
        self.end_key_1 = ""
        self.in_status_1 = "00"
        self.msg_count_1 = 0
        
        # Queue 2 specifications
        self.time_received_2 = {"hrs": 0, "mins": 0, "secs": 0.0}
        self.source_2 = ""
        self.in_length_2 = 0
        self.end_key_2 = ""
        self.in_status_2 = "00"
        self.msg_count_2 = 0
        
        # Output queue specifications
        self.out_length_1 = 0
        self.out_status_1 = "00"
        self.err_key_1 = ""
        self.out_length_2 = 0
        self.out_status_2 = "00"
        self.err_key_2 = ""
    
    def get_time(self) -> dict:
        """Get current time as hours, minutes, seconds"""
        now = datetime.now()
        hrs = now.hour
        mins = now.minute
        secs = now.second + now.microsecond / 1000000.0
        return {"hrs": hrs, "mins": mins, "secs": secs}
    
    def format_time(self, time_dict: dict) -> str:
        """Format time as HH:MM:SS.SS"""
        return f"{time_dict['hrs']:02d}:{time_dict['mins']:02d}:{time_dict['secs']:05.2f}"
    
    def cm104m_init(self):
        """Initialize the program"""
        # Open output file
        self.print_file = open("report.log", "w", encoding="utf-8")
        
        # Set test ID
        self.test_id = "CM104M     "
        self.id_again = self.test_id
        
        # Write headers
        self.head_routine()
        self.log_header()
        
        # Enable queues (simulated)
        key = "**** X-CARD UNDEFINED ****"
        self.cm_inque_1.enable_input(key)
        self.cm_inque_2.enable_input(key)
        self.cm_outque_1.enable_output(key)
        self.cm_outque_2.enable_output(key)
    
    def head_routine(self):
        """Write header routine"""
        self.print_file.write("\f" + self.ccvs_h_1 + "\n\n\n")
        self.print_file.write(self.ccvs_h_2 + "\n\n")
        self.print_file.write(self.ccvs_h_3 + "\n\n\n\n\n")
        self.print_file.write(self.hyphen_line + "\n")
    
    def log_header(self):
        """Write log header"""
        self.print_file.write("\n\n\n" + self.log_hdr_1 + "\n\n\n")
        self.print_file.write(self.log_hdr_2 + "\n\n\n")
        self.print_file.write(self.log_hdr_3 + "\n")
        self.print_file.write(self.log_hdr_4 + "\n")
        self.print_file.write(" " * 120 + "\n")
        self.print_file.write(" " * 120 + "\n")
    
    def cm104m_poll_1(self):
        """Poll queue 1 - receive from INQUE-1, send to OUTQUE-2"""
        # Clear message
        self.msg = " " * 72
        
        # Receive from queue 1
        received_msg = self.cm_inque_1.receive_message()
        if received_msg is None:
            return False  # No data, go to poll 2
        
        self.msg = (received_msg + " " * 72)[:72]
        
        # Accept count from queue 1
        self.msg_count_1 = self.cm_inque_1.get_count()
        
        # Update queue 1 specifications
        if self.cm_inque_1.time_received:
            self.time_received_1 = {
                "hrs": self.cm_inque_1.time_received.hour,
                "mins": self.cm_inque_1.time_received.minute,
                "secs": self.cm_inque_1.time_received.second + 
                       self.cm_inque_1.time_received.microsecond / 1000000.0
            }
        self.source_1 = self.cm_inque_1.source
        self.in_length_1 = self.cm_inque_1.in_length
        self.in_status_1 = self.cm_inque_1.in_status
        
        # Set output length (max 72)
        if self.in_length_1 > 72:
            self.out_length_2 = 72
        else:
            self.out_length_2 = self.in_length_1
        
        # Send to output queue 2
        self.cm_outque_2.out_length = self.out_length_2
        self.cm_outque_2.send_message(self.msg, emi=True)
        
        # Get send time
        self.send_time = self.get_time()
        
        # Build log line
        self.sym_source = self.source_1[:12].ljust(12)
        self.log_time = self.time_received_1
        
        # Compute out time
        send_total_secs = (self.send_time["hrs"] * 3600 + 
                          self.send_time["mins"] * 60 + 
                          self.send_time["secs"])
        received_total_secs = (self.time_received_1["hrs"] * 3600 + 
                              self.time_received_1["mins"] * 60 + 
                              self.time_received_1["secs"])
        self.out_time = send_total_secs - received_total_secs
        
        self.queue_depth = self.msg_count_1
        self.msg_length = self.in_length_1
        self.in_status = self.in_status_1
        self.out_status = self.cm_outque_2.out_status
        self.out_err_key = self.cm_outque_2.err_key
        
        # Format log line
        log_line = (
            " " +
            self.sym_source +
            " " +
            self.format_time(self.log_time) +
            " " +
            f"{self.queue_depth:2d}" +
            f"{self.out_time:6.2f}" +
            " " +
            f"{self.msg_length:3d}" +
            " " +
            self.in_status +
            " " +
            self.out_status +
            "/" +
            self.out_err_key +
            " " +
            self.msg[:68]
        )
        
        # Write log line
        self.print_file.write(log_line + "\n")
        self.print_file.flush()
        
        # Check for kill message
        self.kill_field = self.msg[:4]
        if self.kill_field.upper() == "KILL":
            return True  # Finish
        
        return False  # Continue polling
    
    def cm104m_poll_2(self):
        """Poll queue 2 - receive from INQUE-2, send to OUTQUE-1"""
        # Clear message
        self.msg = " " * 72
        
        # Receive from queue 2
        received_msg = self.cm_inque_2.receive_message()
        if received_msg is None:
            return False  # No data, go back to poll 1
        
        self.msg = (received_msg + " " * 72)[:72]
        
        # Accept count from queue 2
        self.msg_count_2 = self.cm_inque_2.get_count()
        
        # Update queue 2 specifications
        if self.cm_inque_2.time_received:
            self.time_received_2 = {
                "hrs": self.cm_inque_2.time_received.hour,
                "mins": self.cm_inque_2.time_received.minute,
                "secs": self.cm_inque_2.time_received.second + 
                       self.cm_inque_2.time_received.microsecond / 1000000.0
            }
        self.source_2 = self.cm_inque_2.source
        self.in_length_2 = self.cm_inque_2.in_length
        self.in_status_2 = self.cm_inque_2.in_status
        
        # Set output length (max 72)
        if self.in_length_2 > 72:
            self.out_length_1 = 72
        else:
            self.out_length_1 = self.in_length_2
        
        # Send to output queue 1
        self.cm_outque_1.out_length = self.out_length_1
        self.cm_outque_1.send_message(self.msg, emi=True)
        
        # Get send time
        self.send_time = self.get_time()
        
        # Build log line
        self.sym_source = self.source_2[:12].ljust(12)
        self.log_time = self.time_received_2
        
        # Compute out time
        send_total_secs = (self.send_time["hrs"] * 3600 + 
                          self.send_time["mins"] * 60 + 
                          self.send_time["secs"])
        received_total_secs = (self.time_received_2["hrs"] * 3600 + 
                              self.time_received_2["mins"] * 60 + 
                              self.time_received_2["secs"])
        self.out_time = send_total_secs - received_total_secs
        
        self.queue_depth = self.msg_count_2
        self.msg_length = self.in_length_2
        self.in_status = self.in_status_2
        self.out_status = self.cm_outque_1.out_status
        self.out_err_key = self.cm_outque_1.err_key
        
        # Format log line
        log_line = (
            " " +
            self.sym_source +
            " " +
            self.format_time(self.log_time) +
            " " +
            f"{self.queue_depth:2d}" +
            f"{self.out_time:6.2f}" +
            " " +
            f"{self.msg_length:3d}" +
            " " +
            self.in_status +
            " " +
            self.out_status +
            "/" +
            self.out_err_key +
            " " +
            self.msg[:68]
        )
        
        # Write log line
        self.print_file.write(log_line + "\n")
        self.print_file.flush()
        
        # Check for kill message
        self.kill_field = self.msg[:4]
        if self.kill_field.upper() == "KILL":
            return True  # Finish
        
        return False  # Continue polling
    
    def cm104m_fini(self):
        """Finalize the program"""
        self.end_routine()
        if self.print_file:
            self.print_file.close()
    
    def end_routine(self):
        """End routine"""
        self.print_file.write(self.hyphen_line + "\n")
        self.write_line()
        self.para_z()
    
    def para_z(self):
        """Para-Z routine"""
        for _ in range(4):
            self.blank_line_print()
        
        dummy_record = self.ccvs_e_1 + (" " * (120 - len(self.ccvs_e_1)))
        self.print_file.write(dummy_record + "\n")
        self.write_line()
    
    def end_routine_3(self):
        """End routine 3"""
        dummy_record = self.ccvs_e_2 + (" " * (120 - len(self.ccvs_e_2)))
        self.print_file.write(dummy_record + "\n")
        dummy_record = self.ccvs_e_3 + (" " * (120 - len(self.ccvs_e_3)))
        self.print_file.write(dummy_record + "\n")
    
    def blank_line_print(self):
        """Print blank line"""
        self.print_file.write(" " * 120 + "\n")
    
    def write_line(self):
        """Write line"""
        self.print_file.write(" " * 120 + "\n")
    
    def run(self):
        """Main program execution - polling loop"""
        try:
            self.cm104m_init()
            
            # Main polling loop - alternate between queue 1 and queue 2
            while True:
                # Poll queue 1
                if self.cm104m_poll_1():
                    break  # KILL message received
                
                # Poll queue 2
                if self.cm104m_poll_2():
                    break  # KILL message received
                
                # Small delay to prevent busy loop
                time.sleep(0.1)
            
            self.end_routine_3()
            self.cm104m_fini()
            
        except KeyboardInterrupt:
            self.end_routine_3()
            self.cm104m_fini()
        except Exception as e:
            if self.print_file:
                self.print_file.write(f"\nERROR: {e}\n")
                self.print_file.close()


if __name__ == "__main__":
    program = CM104M()
    program.run()

