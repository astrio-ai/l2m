"""
Ground Truth Python Equivalent for MARBLES_COST.cbl (MARBLESC)

Program: MARBLESC
Transaction: MRBC
Function: Manage marble inventory (CREATE, UPDATE, DELETE operations)

Handles marble inventory operations:
- CRE: Create a new marble entry
- UPD: Update existing marble inventory and cost
- DEL: Delete a marble entry
"""

import sys
import sqlite3
from dataclasses import dataclass
from typing import Optional, Tuple
from pathlib import Path


# Constants
BOOLEAN_FALSE = 0
BOOLEAN_TRUE = 1

VERB_CREATE = 'CRE'
VERB_UPDATE = 'UPD'
VERB_DELETE = 'DEL'
CONST_SUCCESS = 'SUCCESS'

ERROR_MARBLE_DNE = 'MRBC001E'  # Marble does not exist
ERROR_MARBLE_EXISTS = 'MRBC002E'  # Marble already exists

# Database file
DB_FILE = 'event_marble.db'


@dataclass
class WorkAreas:
    """Working storage areas."""
    work_inv: int = 0
    work_cost: int = 0
    work_color: str = ""
    work_row_count: int = 0
    result_color_found: int = BOOLEAN_FALSE
    result_operation_success: int = BOOLEAN_FALSE
    result_verb_create: int = BOOLEAN_FALSE
    result_verb_update: int = BOOLEAN_FALSE
    result_verb_delete: int = BOOLEAN_FALSE
    msg_length: int = 74


@dataclass
class InputData:
    """Input data structure."""
    tran_id: str = ""
    verb: str = ""
    color: str = ""
    inv: int = 0
    cost: int = 0


class MarbleDatabase:
    """Database handler for EVENT.MARBLE table."""
    
    def __init__(self, db_path: str = DB_FILE):
        """Initialize database connection and create table if needed."""
        self.db_path = db_path
        self.conn = None
        self._init_database()
    
    def _init_database(self):
        """Initialize database and create table."""
        self.conn = sqlite3.connect(self.db_path)
        cursor = self.conn.cursor()
        
        # Create table if it doesn't exist
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS event_marble (
                COLOR VARCHAR(10) NOT NULL PRIMARY KEY,
                INVENTORY INTEGER NOT NULL,
                COST INTEGER NOT NULL
            )
        """)
        self.conn.commit()
    
    def check_color_exists(self, color: str) -> bool:
        """Check if a color exists in the database.
        
        Equivalent to COBOL CHECK-IF-COLOR-FOUND.
        
        Args:
            color: Color to check
            
        Returns:
            True if color exists, False otherwise
        """
        cursor = self.conn.cursor()
        cursor.execute(
            "SELECT COUNT(*) FROM event_marble WHERE COLOR = ?",
            (color,)
        )
        count = cursor.fetchone()[0]
        return count > 0
    
    def insert_color(self, color: str, inventory: int, cost: int) -> bool:
        """Insert a new marble color.
        
        Equivalent to COBOL INSERT-COLOR.
        
        Args:
            color: Color name
            inventory: Inventory count
            cost: Cost per marble
            
        Returns:
            True if successful, False otherwise
        """
        try:
            cursor = self.conn.cursor()
            cursor.execute(
                "INSERT INTO event_marble (COLOR, INVENTORY, COST) VALUES (?, ?, ?)",
                (color, inventory, cost)
            )
            self.conn.commit()
            return True
        except sqlite3.IntegrityError:
            # Primary key violation (color already exists)
            return False
        except Exception:
            return False
    
    def update_color(self, color: str, inventory: int, cost: int) -> bool:
        """Update an existing marble color.
        
        Equivalent to COBOL UPDATE-COLOR.
        
        Args:
            color: Color name
            inventory: New inventory count
            cost: New cost per marble
            
        Returns:
            True if successful, False otherwise
        """
        try:
            cursor = self.conn.cursor()
            cursor.execute(
                "UPDATE event_marble SET INVENTORY = ?, COST = ? WHERE COLOR = ?",
                (inventory, cost, color)
            )
            self.conn.commit()
            return cursor.rowcount > 0
        except Exception:
            return False
    
    def delete_color(self, color: str) -> bool:
        """Delete a marble color.
        
        Equivalent to COBOL DELETE-COLOR.
        
        Args:
            color: Color name to delete
            
        Returns:
            True if successful, False otherwise
        """
        try:
            cursor = self.conn.cursor()
            cursor.execute(
                "DELETE FROM event_marble WHERE COLOR = ?",
                (color,)
            )
            self.conn.commit()
            return cursor.rowcount > 0
        except Exception:
            return False
    
    def close(self):
        """Close database connection."""
        if self.conn:
            self.conn.close()


class MarblesCostProgram:
    """Main program class for MARBLESC."""
    
    def __init__(self, db_path: str = DB_FILE):
        """Initialize the program."""
        self.db = MarbleDatabase(db_path)
        self.work = WorkAreas()
        self.input_data = InputData()
        self.output = ""
    
    def init_work_areas(self):
        """Initialize working areas to known values.
        
        Equivalent to COBOL INIT-WORK-AREAS.
        """
        self.work = WorkAreas()
        self.input_data = InputData()
        self.output = ""
    
    def get_trans_input(self, command_line_args: list) -> str:
        """Get transaction input from command line.
        
        Equivalent to COBOL GET-TRANS-INPUT.
        In COBOL, this uses CICS RECEIVE, but we'll use command line args.
        
        Args:
            command_line_args: Command line arguments
            
        Returns:
            Input string
        """
        # Join command line args (skip script name)
        return ' '.join(command_line_args[1:]) if len(command_line_args) > 1 else ""
    
    def parse_cics_input(self, input_string: str):
        """Parse the transaction input.
        
        Equivalent to COBOL PARSE-CICS-INPUT.
        
        Args:
            input_string: Input string to parse
        """
        parts = input_string.strip().split()
        
        if len(parts) >= 1:
            self.input_data.tran_id = parts[0]
        if len(parts) >= 2:
            self.input_data.verb = parts[1]
        if len(parts) >= 3:
            self.input_data.color = parts[2]
        if len(parts) >= 4:
            try:
                self.input_data.inv = int(parts[3])
            except ValueError:
                self.input_data.inv = 0
        if len(parts) >= 5:
            try:
                self.input_data.cost = int(parts[4])
            except ValueError:
                self.input_data.cost = 0
    
    def verify_verb(self):
        """Verify that the verb is valid.
        
        Equivalent to COBOL VERIFY-VERB.
        """
        verb = self.input_data.verb.upper()
        
        if verb == VERB_CREATE:
            self.work.result_verb_create = BOOLEAN_TRUE
        elif verb == VERB_UPDATE:
            self.work.result_verb_update = BOOLEAN_TRUE
        elif verb == VERB_DELETE:
            self.work.result_verb_delete = BOOLEAN_TRUE
        else:
            self.work.msg_length = 41
            self.output = 'USE CRE|UPD|DEL'
    
    def check_if_color_found(self):
        """Check if the input color is found in database.
        
        Equivalent to COBOL CHECK-IF-COLOR-FOUND.
        """
        if self.db.check_color_exists(self.input_data.color):
            self.work.result_color_found = BOOLEAN_TRUE
        else:
            self.work.result_color_found = BOOLEAN_FALSE
    
    def insert_color(self):
        """Insert a new color.
        
        Equivalent to COBOL INSERT-COLOR.
        """
        self.work.work_inv = self.input_data.inv
        self.work.work_cost = self.input_data.cost
        
        success = self.db.insert_color(
            self.input_data.color,
            self.work.work_inv,
            self.work.work_cost
        )
        
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE
    
    def update_color(self):
        """Update an existing color.
        
        Equivalent to COBOL UPDATE-COLOR.
        """
        self.work.work_inv = self.input_data.inv
        self.work.work_cost = self.input_data.cost
        
        success = self.db.update_color(
            self.input_data.color,
            self.work.work_inv,
            self.work.work_cost
        )
        
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE
    
    def delete_color(self):
        """Delete a color.
        
        Equivalent to COBOL DELETE-COLOR.
        """
        success = self.db.delete_color(self.input_data.color)
        
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE
    
    def output_success(self):
        """Output success message.
        
        Equivalent to COBOL OUTPUT-SUCCESS.
        """
        self.work.msg_length = 7
        self.output = CONST_SUCCESS
    
    def output_marble_does_not_exist(self):
        """Output error message for marble not found.
        
        Equivalent to COBOL OUTPUT-MARBLE-DOES-NOT-EXIST.
        """
        self.work.msg_length = 33
        self.output = f"{ERROR_MARBLE_DNE} UNKNOWN COLOR, CREate IT"
    
    def output_marble_already_exists(self):
        """Output error message for marble already exists.
        
        Equivalent to COBOL OUTPUT-MARBLE-ALREADY-EXISTS.
        """
        self.work.msg_length = 51
        self.output = f"{ERROR_MARBLE_EXISTS} MARBLE ALREADY EXISTS, UPDate or DELete IT"
    
    def write_output(self):
        """Write output to stdout.
        
        Equivalent to COBOL WRITE-OUTPUT.
        """
        print(self.output[:self.work.msg_length])
    
    def run(self, command_line_args: list) -> str:
        """Run the main program logic.
        
        Equivalent to COBOL PROCEDURE DIVISION.
        
        Args:
            command_line_args: Command line arguments
            
        Returns:
            Output string
        """
        # Initialize working areas
        self.init_work_areas()
        
        # Get transaction input
        input_string = self.get_trans_input(command_line_args)
        
        # Parse the user input
        self.parse_cics_input(input_string)
        
        # Verify known input verb
        self.verify_verb()
        
        # Route to specific verb processing routine
        if self.work.result_verb_create == BOOLEAN_TRUE:
            self.check_if_color_found()
            if self.work.result_color_found == BOOLEAN_FALSE:
                self.insert_color()
                if self.work.result_operation_success == BOOLEAN_TRUE:
                    self.output_success()
            else:
                self.output_marble_already_exists()
        
        elif self.work.result_verb_update == BOOLEAN_TRUE:
            self.check_if_color_found()
            if self.work.result_color_found == BOOLEAN_TRUE:
                self.update_color()
                if self.work.result_operation_success == BOOLEAN_TRUE:
                    self.output_success()
            else:
                self.output_marble_does_not_exist()
        
        elif self.work.result_verb_delete == BOOLEAN_TRUE:
            self.check_if_color_found()
            if self.work.result_color_found == BOOLEAN_TRUE:
                self.delete_color()
                if self.work.result_operation_success == BOOLEAN_TRUE:
                    self.output_success()
            else:
                self.output_marble_does_not_exist()
        
        # Write output
        self.write_output()
        
        return self.output
    
    def cleanup(self):
        """Cleanup resources."""
        self.db.close()


def main():
    """Main entry point."""
    program = MarblesCostProgram()
    try:
        program.run(sys.argv)
    finally:
        program.cleanup()
    return 0


if __name__ == '__main__':
    sys.exit(main())

