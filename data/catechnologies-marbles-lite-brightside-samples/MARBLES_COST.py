import sqlite3
import sys

# Constants
BOOLEAN_FALSE = 0
BOOLEAN_TRUE = 1
VERB_CREATE = 'CRE'
VERB_UPDATE = 'UPD'
VERB_DELETE = 'DEL'
ERROR_MARBLE_DNE = 'MRBC001E'
ERROR_MARBLE_EXISTS = 'MRBC002E'
CONST_SUCCESS = 'SUCCESS'

class WorkAreas:
    """Working storage areas."""
    def __init__(self):
        self.work_inv = 0
        self.work_cost = 0
        self.work_color = ""
        self.work_row_count = 0
        self.result_color_found = BOOLEAN_FALSE
        self.result_operation_success = BOOLEAN_FALSE
        self.result_verb_create = BOOLEAN_FALSE
        self.result_verb_update = BOOLEAN_FALSE
        self.result_verb_delete = BOOLEAN_FALSE
        self.msg_length = 74

class InputData:
    """Input data structure."""
    def __init__(self):
        self.tran_id = ""
        self.verb = ""
        self.color = ""
        self.inv = 0
        self.cost = 0

class MarbleDatabase:
    """Database handler for EVENT.MARBLE table."""
    
    def __init__(self, db_path='marble_inventory.db'):
        self.db_path = db_path
        self.conn = sqlite3.connect(self.db_path)
        self.init_db()

    def init_db(self):
        with self.conn:
            self.conn.execute("""
                CREATE TABLE IF NOT EXISTS event_marble (
                    color TEXT PRIMARY KEY,
                    inventory INTEGER NOT NULL,
                    cost INTEGER NOT NULL
                )
            """)

    def insert_color(self, color, inventory, cost):
        try:
            with self.conn:
                self.conn.execute("INSERT INTO event_marble (color, inventory, cost) VALUES (?, ?, ?)", (color, inventory, cost))
            return BOOLEAN_TRUE
        except sqlite3.IntegrityError:
            return BOOLEAN_FALSE

    def update_color(self, color, inventory, cost):
        with self.conn:
            cursor = self.conn.execute("UPDATE event_marble SET inventory = ?, cost = ? WHERE color = ?", (inventory, cost, color))
            return cursor.rowcount > 0

    def delete_color(self, color):
        with self.conn:
            cursor = self.conn.execute("DELETE FROM event_marble WHERE color = ?", (color,))
            return cursor.rowcount > 0

    def check_color_exists(self, color):
        cursor = self.conn.execute("SELECT COUNT(*) FROM event_marble WHERE color = ?", (color,))
        return cursor.fetchone()[0] > 0

    def close(self):
        self.conn.close()

class MarblesCostProgram:
    """Main program class for MARBLESC."""
    
    def __init__(self):
        self.db = MarbleDatabase()
        self.work = WorkAreas()
        self.input_data = InputData()
        self.output = ""

    def init_work_areas(self):
        """Initialize working areas to known values."""
        self.work = WorkAreas()
        self.input_data = InputData()
        self.output = ""

    def get_trans_input(self, command_line_args):
        """Get transaction input from command line."""
        return ' '.join(command_line_args[1:]) if len(command_line_args) > 1 else ""

    def parse_cics_input(self, input_string):
        """Parse the transaction input."""
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
        """Verify that the verb is valid."""
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
        """Check if the input color is found in the database."""
        if self.db.check_color_exists(self.input_data.color):
            self.work.result_color_found = BOOLEAN_TRUE
        else:
            self.work.result_color_found = BOOLEAN_FALSE

    def insert_color(self):
        """Insert a new color."""
        success = self.db.insert_color(
            self.input_data.color,
            self.input_data.inv,
            self.input_data.cost
        )
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE

    def update_color(self):
        """Update an existing color."""
        success = self.db.update_color(
            self.input_data.color,
            self.input_data.inv,
            self.input_data.cost
        )
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE

    def delete_color(self):
        """Delete a color."""
        success = self.db.delete_color(self.input_data.color)
        if success:
            self.work.result_operation_success = BOOLEAN_TRUE

    def output_success(self):
        """Output success message."""
        self.work.msg_length = 7
        self.output = CONST_SUCCESS

    def output_marble_does_not_exist(self):
        """Output error message for marble not found."""
        self.work.msg_length = 33
        self.output = f"{ERROR_MARBLE_DNE} UNKNOWN COLOR, CREate IT"

    def output_marble_already_exists(self):
        """Output error message for marble already exists."""
        self.work.msg_length = 51
        self.output = f"{ERROR_MARBLE_EXISTS} MARBLE ALREADY EXISTS, UPDate or DELete IT"

    def write_output(self):
        """Write output to stdout."""
        print(self.output[:self.work.msg_length])

    def run(self, command_line_args):
        """Run the main program logic."""
        self.init_work_areas()
        input_string = self.get_trans_input(command_line_args)
        self.parse_cics_input(input_string)
        self.verify_verb()

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

        self.write_output()

def main():
    """Main entry point."""
    program = MarblesCostProgram()
    program.run(sys.argv)
    program.db.close()

if __name__ == '__main__':
    main()