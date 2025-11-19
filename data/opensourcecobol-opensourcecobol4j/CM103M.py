"""
CM103M - Communication Test Program
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
    
    def __init__(self, main_queue: str = ""):
        self.main_queue = main_queue
        self.no_spec_1 = ""
        self.no_spec_2 = ""
        self.no_spec_3 = ""
        self.time_received = None
        self.in_length = 0
        self.end_key = ""
        self.in_status = "00"
        self.enabled = False
    
    def enable_input(self, key: str):
        """Enable input queue"""
        self.main_queue = key
        self.enabled = True
    
    def receive_message(self) -> str:
        """Receive message from queue (simulated via stdin)"""
        if not self.enabled:
            raise RuntimeError("Queue not enabled")
        
        try:
            message = input().strip()
            self.time_received = datetime.now()
            self.in_length = len(message)
            self.in_status = "00"
            return message
        except EOFError:
            self.in_status = "99"
            return ""
        except Exception:
            self.in_status = "99"
            return ""


class OutputQueue:
    """Simulates COBOL communication queue for output"""
    
    def __init__(self):
        self.one = 1
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
            return False


class CM103M:
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
        
        self.ccvs_pgm_id = "CM103M"
        self.test_id = ""
        self.id_again = ""
        
        # Time structures
        self.mcs_time = {"hrs": 0, "mins": 0, "secs": 0.0}
        self.in_time = {"hrs": 0, "mins": 0, "secs": 0.0}
        self.out_time = {"hrs": 0, "mins": 0, "secs": 0.0}
        
        # Log headers
        self.log_hdr_1 = " " * 54 + "MESSAGE LOG"
        self.log_hdr_2 = (" MCS RECEIPT" + 
                         "PROGRAM" + 
                         "MCS REC" + 
                         "RECV SEND" + 
                         " " * 38 + 
                         "MESSAGE")
        self.log_hdr_3 = ("   " + 
                         "INBOUND" + 
                         "RECEIPT" + 
                         "OUTB\"ND" + 
                         "STAT STAT" + 
                         " " * 39 + 
                         "CONTENT")
        self.log_hdr_4 = (" " + 
                         "-" * 11 + 
                         " " + 
                         "-" * 7 + 
                         " " + 
                         "-" * 7 + 
                         "  " + 
                         "---- ----" + 
                         "-" * 5 + 
                         "  " + 
                         "-" * 72)
        
        # Message structures
        self.msg = " " * 72
        self.kill_field = ""
        
        # File handle
        self.print_file = None
        
        # Communication queues
        self.cm_inque_1 = CommunicationQueue()
        self.cm_outque_1 = OutputQueue()
        
        # Output queue specifications
        self.one = 1
        self.out_length = 0
        self.out_status = "00"
        self.err_key = ""
        self.sym_dest = "**** X-CARD UNDEFINED ****"
    
    def get_time(self) -> dict:
        """Get current time as hours, minutes, seconds"""
        now = datetime.now()
        total_seconds = now.hour * 3600 + now.minute * 60 + now.second + now.microsecond / 1000000
        hrs = now.hour
        mins = now.minute
        secs = now.second + now.microsecond / 1000000.0
        return {"hrs": hrs, "mins": mins, "secs": secs}
    
    def format_time(self, time_dict: dict) -> str:
        """Format time as HH:MM:SS.SS"""
        return f"{time_dict['hrs']:02d}:{time_dict['mins']:02d}:{time_dict['secs']:05.2f}"
    
    def cm103m_init(self):
        """Initialize the program"""
        # Open output file
        self.print_file = open("report.log", "w", encoding="utf-8")
        
        # Set test ID
        self.test_id = "CM103M     "
        self.id_again = self.test_id
        
        # Initialize queue specifications
        self.cm_inque_1.no_spec_1 = ""
        self.cm_inque_1.no_spec_2 = ""
        self.cm_inque_1.no_spec_3 = ""
        self.cm_inque_1.main_queue = "**** X-CARD UNDEFINED ****"
        
        # Enable queues (simulated)
        self.cm_inque_1.enable_input("**** X-CARD UNDEFINED ****")
        self.cm_outque_1.enable_output("**** X-CARD UNDEFINED ****")
        
        # Write headers
        self.head_routine()
        self.log_header()
    
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
    
    def receive_echo_and_log(self):
        """Main receive-echo-log loop"""
        while True:
            # Clear message
            self.msg = " " * 72
            
            # Receive message (from stdin in simulation)
            try:
                received_msg = self.cm_inque_1.receive_message()
                if not received_msg:
                    break
                self.msg = (received_msg + " " * 72)[:72]
            except:
                break
            
            # Get receive time
            self.in_time = self.get_time()
            
            # Set output length (max 72)
            if self.cm_inque_1.in_length > 72:
                self.out_length = 72
            else:
                self.out_length = self.cm_inque_1.in_length
            
            # Send message (echo back)
            self.cm_outque_1.out_length = self.out_length
            self.cm_outque_1.send_message(self.msg, emi=True)
            
            # Get send time
            self.out_time = self.get_time()
            
            # Calculate time differences
            if self.cm_inque_1.time_received:
                time_received_dict = {
                    "hrs": self.cm_inque_1.time_received.hour,
                    "mins": self.cm_inque_1.time_received.minute,
                    "secs": self.cm_inque_1.time_received.second + 
                           self.cm_inque_1.time_received.microsecond / 1000000.0
                }
                self.mcs_time = time_received_dict
            else:
                self.mcs_time = self.in_time
            
            # Compute program time
            in_total_secs = (self.in_time["hrs"] * 3600 + 
                           self.in_time["mins"] * 60 + 
                           self.in_time["secs"])
            mcs_total_secs = (self.mcs_time["hrs"] * 3600 + 
                            self.mcs_time["mins"] * 60 + 
                            self.mcs_time["secs"])
            prog_time = in_total_secs - mcs_total_secs
            
            # Compute time sent
            out_total_secs = (self.out_time["hrs"] * 3600 + 
                            self.out_time["mins"] * 60 + 
                            self.out_time["secs"])
            time_sent = out_total_secs - mcs_total_secs
            
            # Format time record
            time_rec_str = self.format_time(self.mcs_time)
            
            # Build log line
            log_line = (
                " " +
                time_rec_str +
                " " +
                f"{prog_time:6.2f}" +
                "  " +
                f"{time_sent:6.2f}" +
                "    " +
                f"{self.cm_inque_1.in_status:2s}" +
                "  " +
                f"{self.cm_outque_1.out_status:2s}" +
                "/" +
                f"{self.cm_outque_1.err_key:1s}" +
                "   " +
                f"{self.cm_inque_1.in_length:3d}" +
                "   " +
                self.msg[:68]
            )
            
            # Write log line
            self.print_file.write(log_line + "\n")
            self.print_file.flush()
            
            # Check for kill message
            self.kill_field = self.msg[:4]
            if self.kill_field.upper() == "KILL":
                break
        
        # End routine
        self.end_routine()
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
        """Main program execution"""
        try:
            self.cm103m_init()
            self.receive_echo_and_log()
            self.end_routine_3()
        except KeyboardInterrupt:
            self.end_routine_3()
            if self.print_file:
                self.print_file.close()
        except Exception as e:
            if self.print_file:
                self.print_file.write(f"\nERROR: {e}\n")
                self.print_file.close()


if __name__ == "__main__":
    program = CM103M()
    program.run()

