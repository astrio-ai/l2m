```python
#!/usr/bin/env python3
import logging
import datetime
from pathlib import Path
from typing import Dict, Any

from data_structures import (
    PrintRecord, 
    AccountRecord,
    Headers,
    Trailers,
    WorkingStorage
)
from file_operations import FileHandler
from procedures import AccountProcedures

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('cbl0009.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class CBL0009:
    """Main application class that implements the COBOL program logic"""
    
    def __init__(self, input_file: str, output_file: str):
        """Initialize the application with input and output file paths"""
        self.file_handler = FileHandler(input_file, output_file)
        self.procedures = AccountProcedures()
        self.working_storage = WorkingStorage()
        self.headers = Headers()
        self.trailers = Trailers()
        
    def write_headers(self) -> None:
        """Write report headers with current date"""
        try:
            current_date = datetime.datetime.now()
            self.headers.header2.year = current_date.year
            self.headers.header2.month = current_date.month
            self.headers.header2.day = current_date.day
            
            self.file_handler.write_record(self.headers.header1)
            self.file_handler.write_record(self.headers.header2)
            self.file_handler.write_record(PrintRecord()) # blank line
            self.file_handler.write_record(self.headers.header3)
            self.file_handler.write_record(self.headers.header4)
            self.file_handler.write_record(PrintRecord()) # blank line
            
        except Exception as e:
            logger.error(f"Error writing headers: {str(e)}")
            raise
            
    def process_records(self) -> None:
        """Process all account records and calculate totals"""
        try:
            while not self.working_storage.last_record:
                record = self.file_handler.read_account_record()
                
                if record is None:
                    self.working_storage.last_record = True
                    continue
                    
                # Calculate running totals
                self.procedures.calculate_totals(
                    record,
                    self.working_storage.total_limit,
                    self.working_storage.total_balance
                )
                
                # Write detail record
                print_record = PrintRecord(
                    account_no=record.account_no,
                    last_name=record.last_name,
                    account_limit=record.account_limit,
                    account_balance=record.account_balance
                )
                self.file_handler.write_record(print_record)
                
        except Exception as e:
            logger.error(f"Error processing records: {str(e)}")
            raise
            
    def write_totals(self) -> None:
        """Write trailer records with totals"""
        try:
            self.trailers.trailer2.total_limit = self.working_storage.total_limit
            self.trailers.trailer2.total_balance = self.working_storage.total_balance
            
            self.file_handler.write_record(self.trailers.trailer1)
            self.file_handler.write_record(self.trailers.trailer2)
            
        except Exception as e:
            logger.error(f"Error writing totals: {str(e)}")
            raise
            
    def run(self) -> None:
        """Main program execution flow"""
        logger.info("Starting CBL0009 processing")
        
        try:
            # Open files
            self.file_handler.open_files()
            
            # Process report
            self.write_headers()
            self.process_records()
            self.write_totals()
            
            # Close files
            self.file_handler.close_files()
            
            logger.info("CBL0009 processing completed successfully")
            
        except Exception as e:
            logger.error(f"Error in CBL0009 processing: {str(e)}")
            raise
        finally:
            self.file_handler.cleanup()

def main():
    """Main entry point"""
    try:
        # Configure input/output files
        input_file = "ACCTREC"
        output_file = "PRTLINE"
        
        # Create and run application
        app = CBL0009(input_file, output_file)
        app.run()
        
    except Exception as e:
        logger.critical(f"Application error: {str(e)}")
        raise SystemExit(1)

if __name__ == "__main__":
    main()
```