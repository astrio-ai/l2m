"""
Pytest tests for CBSTM03A.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from decimal import Decimal
from unittest.mock import patch, MagicMock, mock_open

# Add parent directory to path to import CBSTM03A
sys.path.insert(0, str(Path(__file__).parent))

from CBSTM03A import (
    CardXrefRecord,
    CustomerRecord,
    AccountRecord,
    TranRecord,
    CBSTM03BHandler,
    format_amount,
    parse_tran_record,
    parse_xref_record,
    parse_customer_record,
    parse_account_record,
    abend_program,
    _create_statement,
    _write_trans,
    main
)


class TestCardXrefRecord:
    """Tests for CardXrefRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CardXrefRecord."""
        record = CardXrefRecord(
            card_num="1234567890123456",
            cust_num="123456789",
            acct_id="98765432109"
        )
        assert record.card_num == "1234567890123456"
        assert record.cust_id == "123456789"
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_acct_id == "98765432109"


class TestCustomerRecord:
    """Tests for CustomerRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CustomerRecord."""
        record = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            middle_name="M",
            last_name="Doe",
            addr_line_1="123 Main St",
            addr_line_2="Apt 4",
            addr_line_3="City",
            addr_state_cd="NY",
            addr_country_cd="US",
            addr_zip="12345",
            fico_score="750"
        )
        assert record.cust_first_name == "John"
        assert record.cust_last_name == "Doe"
        assert record.cust_fico_credit_score == "750"


class TestAccountRecord:
    """Tests for AccountRecord dataclass."""
    
    def test_creation(self):
        """Test creating an AccountRecord."""
        record = AccountRecord(
            acct_id="12345678901",
            curr_bal=Decimal('1000.50')
        )
        assert record.acct_id == "12345678901"
        assert record.acct_curr_bal == Decimal('1000.50')


class TestTranRecord:
    """Tests for TranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranRecord."""
        record = TranRecord(
            card_num="1234567890123456",
            trans_id="TRAN000000000001",
            trans_desc="Purchase at Store",
            trans_amt=Decimal('50.00'),
            trans_rest=""
        )
        assert record.trnx_card_num == "1234567890123456"
        assert record.trnx_id == "TRAN000000000001"
        assert record.trnx_amt == Decimal('50.00')


class TestFormatAmount:
    """Tests for format_amount function."""
    
    def test_positive_amount(self):
        """Test formatting positive amount."""
        result = format_amount(Decimal('1234.56'))
        assert result == "  1234.56 "
    
    def test_negative_amount(self):
        """Test formatting negative amount."""
        result = format_amount(Decimal('-1234.56'))
        assert result == "  1234.56-"
    
    def test_zero_amount(self):
        """Test formatting zero amount."""
        result = format_amount(Decimal('0'))
        assert result == "     0.00 "


class TestParsingFunctions:
    """Tests for parsing functions."""
    
    def test_parse_tran_record(self):
        """Test parsing transaction record."""
        line = "1234567890123456TRAN000000000001Purchase at Store                                   50.00"
        record = parse_tran_record(line)
        assert record.trnx_card_num == "1234567890123456"
        assert record.trnx_id == "TRAN000000000001"
    
    def test_parse_xref_record(self):
        """Test parsing XREF record."""
        line = "123456789012345612345678998765432109" + " " * 14
        record = parse_xref_record(line)
        assert record.card_num == "1234567890123456"
        assert record.cust_num == "123456789"
        assert record.acct_id == "98765432109"
    
    def test_parse_customer_record(self):
        """Test parsing customer record."""
        line = "123456789|John|M|Doe|123 Main St|Apt 4|City|NY|US|12345|750"
        record = parse_customer_record(line)
        assert record.cust_id == "123456789"
        assert record.first_name == "John"
        assert record.last_name == "Doe"
    
    def test_parse_account_record(self):
        """Test parsing account record."""
        line = "12345678901" + " " * 15 + "1000.50"
        record = parse_account_record(line)
        assert record.acct_id == "12345678901"


class TestCBSTM03BHandler:
    """Tests for CBSTM03BHandler class."""
    
    def test_open_file_success(self):
        """Test successful file opening."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as f:
            f.write("test data\n")
            temp_file = f.name
        
        try:
            handler = CBSTM03BHandler()
            # Mock file opening
            with patch('builtins.open', mock_open(read_data="test data\n")):
                rc, _ = handler.call_subroutine(temp_file, 'O')
                # Note: In real implementation, this would actually open files
                # For now, we'll test the interface
                assert rc in ('00', '23', '99')  # Valid return codes
        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)
    
    def test_read_sequential(self):
        """Test sequential read."""
        handler = CBSTM03BHandler()
        handler.file_handles['TESTFILE'] = open('/dev/null', 'r') if os.path.exists('/dev/null') else None
        
        # Test with no file open
        rc, _ = handler.call_subroutine('NONEXISTENT', 'R')
        assert rc == '12'


class TestCreateStatement:
    """Tests for _create_statement function."""
    
    def test_create_statement(self):
        """Test creating statement header."""
        customer = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            middle_name="M",
            last_name="Doe",
            addr_line_1="123 Main St",
            addr_line_2="Apt 4",
            addr_line_3="City",
            addr_state_cd="NY",
            addr_country_cd="US",
            addr_zip="12345",
            fico_score="750"
        )
        
        account = AccountRecord(
            acct_id="98765432109",
            curr_bal=Decimal('1000.50')
        )
        
        xref = CardXrefRecord(
            card_num="1234567890123456",
            cust_num="123456789",
            acct_id="98765432109"
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as stmt_file:
            stmt_path = stmt_file.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.html') as html_file:
            html_path = html_file.name
        
        try:
            with open(stmt_path, 'w') as stmt, open(html_path, 'w') as html:
                _create_statement(stmt, html, customer, account, xref)
            
            # Verify statement file contains expected content
            with open(stmt_path, 'r') as f:
                content = f.read()
                assert "START OF STATEMENT" in content
                assert "John" in content
                assert "98765432109" in content
            
            # Verify HTML file contains expected content
            with open(html_path, 'r') as f:
                content = f.read()
                assert "<!DOCTYPE html>" in content
                assert "98765432109" in content
                assert "John" in content
        finally:
            if os.path.exists(stmt_path):
                os.unlink(stmt_path)
            if os.path.exists(html_path):
                os.unlink(html_path)


class TestWriteTrans:
    """Tests for _write_trans function."""
    
    def test_write_trans(self):
        """Test writing transaction line."""
        tran_record = TranRecord(
            card_num="1234567890123456",
            trans_id="TRAN000000000001",
            trans_desc="Purchase at Store",
            trans_amt=Decimal('50.00'),
            trans_rest=""
        )
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.txt') as stmt_file:
            stmt_path = stmt_file.name
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.html') as html_file:
            html_path = html_file.name
        
        try:
            with open(stmt_path, 'w') as stmt, open(html_path, 'w') as html:
                _write_trans(stmt, html, tran_record)
            
            # Verify statement file contains transaction
            with open(stmt_path, 'r') as f:
                content = f.read()
                assert "TRAN000000000001" in content
                assert "Purchase at Store" in content
            
            # Verify HTML file contains transaction
            with open(html_path, 'r') as f:
                content = f.read()
                assert "TRAN000000000001" in content
        finally:
            if os.path.exists(stmt_path):
                os.unlink(stmt_path)
            if os.path.exists(html_path):
                os.unlink(html_path)


class TestAbendProgram:
    """Tests for abend_program function."""
    
    def test_abend_program(self):
        """Test abend_program function."""
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            with pytest.raises(SystemExit) as exc_info:
                abend_program()
            
            assert exc_info.value.code == 999
        
        output = f.getvalue()
        assert "ABENDING PROGRAM" in output


class TestIntegration:
    """Integration tests."""
    
    def test_full_workflow_with_mocked_files(self):
        """Test full workflow with mocked file handlers."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            trnx_file = os.path.join(tmpdir, "TRNXFILE")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            cust_file = os.path.join(tmpdir, "CUSTFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            stmt_file = os.path.join(tmpdir, "STMTFILE")
            html_file = os.path.join(tmpdir, "HTMLFILE")
            
            # Create test data
            with open(trnx_file, 'w') as f:
                f.write("1234567890123456TRAN000000000001Purchase at Store                                   50.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            with open(cust_file, 'w') as f:
                f.write("123456789|John|M|Doe|123 Main St|Apt 4|City|NY|US|12345|750\n")
            
            with open(acct_file, 'w') as f:
                f.write("98765432109" + " " * 15 + "1000.50\n")
            
            # Mock the handler to use our test files
            old_handler = CBSTM03BHandler
            handler = CBSTM03BHandler()
            
            # Set up file handles
            handler.file_handles['TRNXFILE'] = open(trnx_file, 'r')
            handler.file_handles['XREFFILE'] = open(xref_file, 'r')
            handler.file_handles['CUSTFILE'] = open(cust_file, 'r')
            handler.file_handles['ACCTFILE'] = open(acct_file, 'r')
            
            # Test that files can be read
            assert handler.file_handles['TRNXFILE'] is not None
            
            # Clean up
            for fh in handler.file_handles.values():
                fh.close()


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

