"""
Pytest tests for CBSTM03B.py ground truth Python equivalent.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from CBSTM03B import (
    M03BArea,
    CBSTM03BSubroutine,
    TrnxFileHandler,
    XrefFileHandler,
    CustFileHandler,
    AcctFileHandler,
    cbstmt03b_subroutine,
    reset_subroutine
)


class TestM03BArea:
    """Tests for M03BArea dataclass."""
    
    def test_creation(self):
        """Test creating M03BArea."""
        area = M03BArea(
            dd_name="TRNXFILE",
            operation="O",
            return_code="00",
            key="123456789",
            key_len=9,
            field_data=""
        )
        assert area.dd_name == "TRNXFILE"
        assert area.is_open is True
        assert area.is_read is False
    
    def test_operation_properties(self):
        """Test operation property checks."""
        area = M03BArea(operation="O")
        assert area.is_open
        assert not area.is_close
        
        area.operation = "C"
        assert area.is_close
        
        area.operation = "R"
        assert area.is_read
        
        area.operation = "K"
        assert area.is_read_key


class TestCBSTM03BSubroutine:
    """Tests for CBSTM03BSubroutine."""
    
    def test_process_trnxfile_open(self):
        """Test opening TRNXFILE."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("test data\n")
            temp_file = f.name
        
        try:
            os.rename(temp_file, "TRNXFILE")
            subroutine = CBSTM03BSubroutine()
            area = M03BArea(dd_name="TRNXFILE", operation="O")
            subroutine.process(area)
            assert area.return_code == "00"
        finally:
            if os.path.exists("TRNXFILE"):
                os.unlink("TRNXFILE")
    
    def test_process_xreffile_read(self):
        """Test reading XREFFILE."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("1234567890123456TEST DATA\n")
            temp_file = f.name
        
        try:
            os.rename(temp_file, "XREFFILE")
            subroutine = CBSTM03BSubroutine()
            area = M03BArea(dd_name="XREFFILE", operation="O")
            subroutine.process(area)
            assert area.return_code == "00"
            
            area.operation = "R"
            area.field_data = ""
            subroutine.process(area)
            assert area.return_code in ("00", "10")
        finally:
            if os.path.exists("XREFFILE"):
                os.unlink("XREFFILE")


def test_cbstmt03b_subroutine():
    """Test the entry point function."""
    reset_subroutine()
    area = M03BArea(dd_name="TRNXFILE", operation="O")
    # Should not raise exception
    cbstmt03b_subroutine(area)

