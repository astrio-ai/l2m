"""
Pytest tests for marbles_cost.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
import sqlite3
from pathlib import Path

# Add parent directory to path to import marbles_cost
test_dir = Path(__file__).parent
sys.path.insert(0, str(test_dir))

# Import with explicit file name handling
import importlib.util
spec = importlib.util.spec_from_file_location("marbles_cost", test_dir / "marbles_cost.py")
marbles_cost_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(marbles_cost_module)

# Import classes and constants from the loaded module
MarblesCostProgram = marbles_cost_module.MarblesCostProgram
MarbleDatabase = marbles_cost_module.MarbleDatabase
InputData = marbles_cost_module.InputData
WorkAreas = marbles_cost_module.WorkAreas
VERB_CREATE = marbles_cost_module.VERB_CREATE
VERB_UPDATE = marbles_cost_module.VERB_UPDATE
VERB_DELETE = marbles_cost_module.VERB_DELETE
ERROR_MARBLE_DNE = marbles_cost_module.ERROR_MARBLE_DNE
ERROR_MARBLE_EXISTS = marbles_cost_module.ERROR_MARBLE_EXISTS
CONST_SUCCESS = marbles_cost_module.CONST_SUCCESS
BOOLEAN_TRUE = marbles_cost_module.BOOLEAN_TRUE
BOOLEAN_FALSE = marbles_cost_module.BOOLEAN_FALSE


class TestMarbleDatabase:
    """Tests for MarbleDatabase class."""
    
    def test_init_database(self):
        """Test database initialization."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            assert db.conn is not None
            
            # Check table was created
            cursor = db.conn.cursor()
            cursor.execute(
                "SELECT name FROM sqlite_master WHERE type='table' AND name='event_marble'"
            )
            result = cursor.fetchone()
            assert result is not None
            
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_check_color_exists_false(self):
        """Test checking for non-existent color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            assert db.check_color_exists("RED") is False
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_insert_color(self):
        """Test inserting a color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            result = db.insert_color("BLUE", 10, 4)
            assert result is True
            assert db.check_color_exists("BLUE") is True
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_insert_color_duplicate(self):
        """Test inserting duplicate color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            db.insert_color("BLUE", 10, 4)
            result = db.insert_color("BLUE", 20, 5)  # Duplicate
            assert result is False
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_update_color(self):
        """Test updating a color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            db.insert_color("BLUE", 10, 4)
            result = db.update_color("BLUE", 1, 5)
            assert result is True
            
            # Verify update
            cursor = db.conn.cursor()
            cursor.execute(
                "SELECT INVENTORY, COST FROM event_marble WHERE COLOR = ?",
                ("BLUE",)
            )
            row = cursor.fetchone()
            assert row[0] == 1
            assert row[1] == 5
            
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_update_color_not_found(self):
        """Test updating non-existent color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            result = db.update_color("RED", 10, 5)
            assert result is False  # Color doesn't exist
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_delete_color(self):
        """Test deleting a color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            db.insert_color("BLUE", 10, 4)
            assert db.check_color_exists("BLUE") is True
            
            result = db.delete_color("BLUE")
            assert result is True
            assert db.check_color_exists("BLUE") is False
            
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_delete_color_not_found(self):
        """Test deleting non-existent color."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            db = MarbleDatabase(db_path)
            result = db.delete_color("RED")
            assert result is False  # Color doesn't exist
            db.close()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)


class TestMarblesCostProgram:
    """Tests for MarblesCostProgram class."""
    
    def _create_temp_db(self):
        """Helper to create temporary database."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        return db_path
    
    def test_init_work_areas(self):
        """Test initialization of work areas."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.init_work_areas()
            
            assert program.work.work_inv == 0
            assert program.work.result_color_found == BOOLEAN_FALSE
            assert program.input_data.verb == ""
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_parse_cics_input(self):
        """Test parsing input string."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.parse_cics_input("MRBC CRE BLUE 10 4")
            
            assert program.input_data.tran_id == "MRBC"
            assert program.input_data.verb == "CRE"
            assert program.input_data.color == "BLUE"
            assert program.input_data.inv == 10
            assert program.input_data.cost == 4
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_verify_verb_create(self):
        """Test verb verification for CREATE."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.input_data.verb = "CRE"
            program.verify_verb()
            
            assert program.work.result_verb_create == BOOLEAN_TRUE
            assert program.work.result_verb_update == BOOLEAN_FALSE
            assert program.work.result_verb_delete == BOOLEAN_FALSE
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_verify_verb_update(self):
        """Test verb verification for UPDATE."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.input_data.verb = "UPD"
            program.verify_verb()
            
            assert program.work.result_verb_create == BOOLEAN_FALSE
            assert program.work.result_verb_update == BOOLEAN_TRUE
            assert program.work.result_verb_delete == BOOLEAN_FALSE
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_verify_verb_delete(self):
        """Test verb verification for DELETE."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.input_data.verb = "DEL"
            program.verify_verb()
            
            assert program.work.result_verb_create == BOOLEAN_FALSE
            assert program.work.result_verb_update == BOOLEAN_FALSE
            assert program.work.result_verb_delete == BOOLEAN_TRUE
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_verify_verb_invalid(self):
        """Test verb verification for invalid verb."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            program.input_data.verb = "INVALID"
            program.verify_verb()
            
            assert program.work.result_verb_create == BOOLEAN_FALSE
            assert program.work.result_verb_update == BOOLEAN_FALSE
            assert program.work.result_verb_delete == BOOLEAN_FALSE
            assert program.output == 'USE CRE|UPD|DEL'
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_create_operation_success(self):
        """Test CREATE operation with success."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            
            result = f.getvalue().strip()
            assert result == CONST_SUCCESS
            assert program.db.check_color_exists("BLUE") is True
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_create_operation_already_exists(self):
        """Test CREATE operation when color already exists."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            # Create first entry
            program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            
            # Try to create again
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "20", "5"])
            
            result = f.getvalue().strip()
            assert ERROR_MARBLE_EXISTS in result
            assert "MARBLE ALREADY EXISTS" in result
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_update_operation_success(self):
        """Test UPDATE operation with success."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            # Create first
            program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            
            # Update
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "UPD", "BLUE", "1", "5"])
            
            result = f.getvalue().strip()
            assert result == CONST_SUCCESS
            
            # Verify update
            cursor = program.db.conn.cursor()
            cursor.execute(
                "SELECT INVENTORY, COST FROM event_marble WHERE COLOR = ?",
                ("BLUE",)
            )
            row = cursor.fetchone()
            assert row[0] == 1
            assert row[1] == 5
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_update_operation_not_found(self):
        """Test UPDATE operation when color doesn't exist."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "UPD", "RED", "10", "5"])
            
            result = f.getvalue().strip()
            assert ERROR_MARBLE_DNE in result
            assert "UNKNOWN COLOR" in result
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_delete_operation_success(self):
        """Test DELETE operation with success."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            # Create first
            program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            
            # Delete
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "DEL", "BLUE"])
            
            result = f.getvalue().strip()
            assert result == CONST_SUCCESS
            assert program.db.check_color_exists("BLUE") is False
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_delete_operation_not_found(self):
        """Test DELETE operation when color doesn't exist."""
        db_path = self._create_temp_db()
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            f = io.StringIO()
            with redirect_stdout(f):
                output = program.run(["marbles_cost.py", "MRBC", "DEL", "RED"])
            
            result = f.getvalue().strip()
            assert ERROR_MARBLE_DNE in result
            assert "UNKNOWN COLOR" in result
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)


class TestIntegration:
    """Integration tests for full workflow."""
    
    def test_full_workflow_create_update_delete(self):
        """Test complete workflow: create, update, delete."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            # Create
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            assert f.getvalue().strip() == CONST_SUCCESS
            
            # Update
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "UPD", "BLUE", "5", "6"])
            assert f.getvalue().strip() == CONST_SUCCESS
            
            # Verify update
            cursor = program.db.conn.cursor()
            cursor.execute(
                "SELECT INVENTORY, COST FROM event_marble WHERE COLOR = ?",
                ("BLUE",)
            )
            row = cursor.fetchone()
            assert row[0] == 5
            assert row[1] == 6
            
            # Delete
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "DEL", "BLUE"])
            assert f.getvalue().strip() == CONST_SUCCESS
            
            # Verify deleted
            assert program.db.check_color_exists("BLUE") is False
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_multiple_colors(self):
        """Test managing multiple colors."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            # Create multiple colors
            colors = ["BLUE", "RED", "GREEN"]
            for color in colors:
                f = io.StringIO()
                with redirect_stdout(f):
                    program.run(["marbles_cost.py", "MRBC", "CRE", color, "10", "4"])
                assert f.getvalue().strip() == CONST_SUCCESS
            
            # Verify all exist
            for color in colors:
                assert program.db.check_color_exists(color) is True
            
            # Delete all
            for color in colors:
                f = io.StringIO()
                with redirect_stdout(f):
                    program.run(["marbles_cost.py", "MRBC", "DEL", color])
                assert f.getvalue().strip() == CONST_SUCCESS
            
            # Verify all deleted
            for color in colors:
                assert program.db.check_color_exists(color) is False
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)
    
    def test_error_scenarios(self):
        """Test various error scenarios."""
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as f:
            db_path = f.name
        
        try:
            program = MarblesCostProgram(db_path)
            
            import io
            from contextlib import redirect_stdout
            
            # Try to update non-existent color
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "UPD", "RED", "10", "5"])
            assert ERROR_MARBLE_DNE in f.getvalue()
            
            # Try to delete non-existent color
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "DEL", "RED"])
            assert ERROR_MARBLE_DNE in f.getvalue()
            
            # Create a color
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "10", "4"])
            assert f.getvalue().strip() == CONST_SUCCESS
            
            # Try to create duplicate
            f = io.StringIO()
            with redirect_stdout(f):
                program.run(["marbles_cost.py", "MRBC", "CRE", "BLUE", "20", "5"])
            assert ERROR_MARBLE_EXISTS in f.getvalue()
            
            program.cleanup()
        finally:
            if os.path.exists(db_path):
                os.unlink(db_path)

