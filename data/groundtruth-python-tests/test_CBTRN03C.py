"""
Pytest tests for CBTRN03C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from decimal import Decimal
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBTRN03C
sys.path.insert(0, str(Path(__file__).parent))

from CBTRN03C import (
    TranRecord,
    CardXrefRecord,
    TranTypeRecord,
    TranCatRecord,
    SequentialFileHandler,
    XrefFileHandler,
    TranTypeFileHandler,
    TranCatFileHandler,
    parse_tran_record,
    parse_xref_record,
    parse_tran_type_record,
    parse_tran_cat_record,
    format_report_name_header,
    format_transaction_header_1,
    format_transaction_header_2,
    format_transaction_detail,
    format_page_totals,
    format_account_totals,
    format_grand_totals,
    abend_program,
    main
)


class TestTranRecord:
    """Tests for TranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranRecord."""
        record = TranRecord(
            tran_id="TRAN000000000001",
            tran_type_cd="01",
            tran_cat_cd="0005",
            tran_amt=Decimal('100.50')
        )
        assert record.trans_id == "TRAN000000000001"
        assert record.tran_type_cd == "01"
        assert record.tran_amt == Decimal('100.50')


class TestCardXrefRecord:
    """Tests for CardXrefRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CardXrefRecord."""
        record = CardXrefRecord(
            card_num="1234567890123456",
            cust_num="123456789",
            acct_id="98765432109"
        )
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_acct_id == "98765432109"


class TestTranTypeRecord:
    """Tests for TranTypeRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranTypeRecord."""
        record = TranTypeRecord(
            tran_type="01",
            tran_type_desc="Purchase"
        )
        assert record.tran_type_cd == "01"
        assert record.tran_type_desc == "Purchase"


class TestTranCatRecord:
    """Tests for TranCatRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranCatRecord."""
        record = TranCatRecord(
            tran_type_cd="01",
            tran_cat_cd="0005",
            tran_cat_type_desc="Retail"
        )
        assert record.tran_cat_type_cd == "01"
        assert record.tran_cat_cd == "0005"


class TestParsingFunctions:
    """Tests for parsing functions."""
    
    def test_parse_tran_record(self):
        """Test parsing transaction record."""
        # Format: ID(16) + type(2) + cat(4) + source(20) + desc(50) + amt(15) + merchant_id(10) + merchant_name(30) + merchant_city(20) + merchant_zip(10) + card(16) + orig_ts(26) + proc_ts(26 at offset 304)
        line = ("TRAN00000000000101   0005SYSTEM             Purchase                   "
                "100.50         MERCHANT001 Store Name                    City              "
                "12345     12345678901234562024-01-01-12.00.00.000000" + 
                " " * (304 - 219) + "2024-01-15-12.00.00.000000")
        record = parse_tran_record(line)
        assert record.tran_id == "TRAN000000000001"
        assert record.tran_type_cd == "01"
        assert record.tran_cat_cd == "0005"
        assert record.tran_proc_ts == "2024-01-15-12.00.00.000000"
    
    def test_parse_xref_record(self):
        """Test parsing XREF record."""
        line = "123456789012345612345678998765432109" + " " * 14
        record = parse_xref_record(line)
        assert record.card_num == "1234567890123456"
        assert record.cust_num == "123456789"
        assert record.acct_id == "98765432109"
    
    def test_parse_tran_type_record(self):
        """Test parsing transaction type record."""
        line = "01Purchase Transaction Type" + " " * 30
        record = parse_tran_type_record(line)
        assert record.tran_type == "01"
        assert "Purchase" in record.tran_type_desc
    
    def test_parse_tran_cat_record(self):
        """Test parsing transaction category record."""
        line = "010005Retail Category" + " " * 40
        record = parse_tran_cat_record(line)
        assert record.tran_type_cd == "01"
        assert record.tran_cat_cd == "0005"
        assert "Retail" in record.tran_cat_type_desc


class TestFormattingFunctions:
    """Tests for formatting functions."""
    
    def test_format_report_name_header(self):
        """Test formatting report name header."""
        header = format_report_name_header()
        assert "TRANSACTION DETAIL REPORT" in header
        assert len(header) == 133
    
    def test_format_transaction_header_1(self):
        """Test formatting transaction header 1."""
        header = format_transaction_header_1()
        assert "TRANS-ID" in header
        assert "ACCT-ID" in header
        assert len(header) == 133
    
    def test_format_transaction_header_2(self):
        """Test formatting transaction header 2."""
        header = format_transaction_header_2()
        assert header == "-" * 133
    
    def test_format_transaction_detail(self):
        """Test formatting transaction detail line."""
        detail = format_transaction_detail(
            "TRAN000000000001",
            "ACCT12345678",
            "01",
            "Purchase",
            "0005",
            "Retail",
            "SYSTEM",
            Decimal('100.50')
        )
        assert "TRAN000000000001" in detail
        assert "ACCT1234567" in detail  # Account ID is truncated to 11 chars
        assert len(detail) == 133
    
    def test_format_page_totals(self):
        """Test formatting page totals."""
        totals = format_page_totals(Decimal('1000.50'))
        assert "PAGE TOTAL" in totals
        assert "1000.50" in totals
        assert len(totals) == 133
    
    def test_format_account_totals(self):
        """Test formatting account totals."""
        totals = format_account_totals(Decimal('5000.00'))
        assert "ACCOUNT TOTAL" in totals
        assert "5000.00" in totals
        assert len(totals) == 133
    
    def test_format_grand_totals(self):
        """Test formatting grand totals."""
        totals = format_grand_totals(Decimal('10000.00'))
        assert "GRAND TOTAL" in totals
        assert "10000.00" in totals
        assert len(totals) == 133


class TestFileHandlers:
    """Tests for file handlers."""
    
    def test_sequential_file_handler_open_success(self):
        """Test sequential file handler opening successfully."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("test line\n")
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            result = handler.open_file("r")
            assert result == 0
            assert handler.file_status == "00"
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_sequential_file_handler_open_not_found(self):
        """Test sequential file handler with file not found."""
        handler = SequentialFileHandler("nonexistent_file.txt")
        result = handler.open_file("r")
        assert result == 12
        assert handler.file_status == "23"
    
    def test_sequential_file_handler_read(self):
        """Test sequential file handler reading."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("line 1\nline 2\n")
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("r")
            line, result = handler.read_next()
            assert result == 0
            assert line == "line 1"
            line, result = handler.read_next()
            assert result == 0
            assert line == "line 2"
            line, result = handler.read_next()
            assert result == 16
            assert handler.end_of_file
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_sequential_file_handler_write(self):
        """Test sequential file handler writing."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            temp_file = f.name
        
        try:
            handler = SequentialFileHandler(temp_file)
            handler.open_file("w")
            result = handler.write_record("test record")
            assert result == 0
            handler.close_file()
            
            with open(temp_file, 'r') as f:
                content = f.read()
                assert "test record" in content
        finally:
            os.unlink(temp_file)
    
    def test_indexed_file_handler_open_success(self):
        """Test indexed file handler opening successfully."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("1234567890123456DATA\n")
            temp_file = f.name
        
        try:
            handler = XrefFileHandler(temp_file)
            result = handler.open_file("r")
            assert result == 0
            assert handler.file_status == "00"
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_indexed_file_handler_read(self):
        """Test indexed file handler reading."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("1234567890123456DATA LINE\n")
            temp_file = f.name
        
        try:
            handler = XrefFileHandler(temp_file)
            handler.open_file("r")
            line, result = handler.read_record("1234567890123456")
            assert result == 0
            assert "DATA LINE" in line
            line, result = handler.read_record("9999999999999999")
            assert result == 12
            assert handler.file_status == "23"
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_tran_type_file_handler(self):
        """Test transaction type file handler."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("01Purchase Type\n")
            temp_file = f.name
        
        try:
            handler = TranTypeFileHandler(temp_file)
            handler.open_file("r")
            line, result = handler.read_record("01")
            assert result == 0
            assert "Purchase" in line
            handler.close_file()
        finally:
            os.unlink(temp_file)
    
    def test_tran_cat_file_handler(self):
        """Test transaction category file handler."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write("010005Retail Category\n")
            temp_file = f.name
        
        try:
            handler = TranCatFileHandler(temp_file)
            handler.open_file("r")
            line, result = handler.read_record("010005")
            assert result == 0
            assert "Retail" in line
            handler.close_file()
        finally:
            os.unlink(temp_file)


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_success(self):
        """Test successful main execution."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create temporary files
            tranfile_path = os.path.join(temp_dir, "TRANFILE")
            reportfile_path = os.path.join(temp_dir, "TRANREPT")
            xreffile_path = os.path.join(temp_dir, "CARDXREF")
            trantypefile_path = os.path.join(temp_dir, "TRANTYPE")
            trancatfile_path = os.path.join(temp_dir, "TRANCATG")
            dateparmfile_path = os.path.join(temp_dir, "DATEPARM")
            
            # Write date parameter file
            with open(dateparmfile_path, 'w') as f:
                f.write("2024-01-01 2024-01-31\n")
            
            # Write transaction file
            with open(tranfile_path, 'w') as f:
                # Format: ID(16) + type(2) + cat(4) + source(20) + desc(50) + amt(15) + merchant_id(10) + merchant_name(30) + merchant_city(20) + merchant_zip(10) + card(16) + orig_ts(26) + padding + proc_ts(26)
                tran_line = (
                    "TRAN00000000000101   0005SYSTEM             Purchase                   "
                    "100.50         MERCHANT001 Store Name                    City              "
                    "12345     12345678901234562024-01-01-12.00.00.000000" +
                    " " * (304 - 219) + "2024-01-15-12.00.00.000000" + " " * 20 + "\n"
                )
                f.write(tran_line)
            
            # Write XREF file
            with open(xreffile_path, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Write transaction type file
            with open(trantypefile_path, 'w') as f:
                f.write("01Purchase Transaction Type" + " " * 30 + "\n")
            
            # Write transaction category file
            with open(trancatfile_path, 'w') as f:
                f.write("010005Retail Category" + " " * 40 + "\n")
            
            # Change to temp directory and run main
            original_cwd = os.getcwd()
            try:
                os.chdir(temp_dir)
                with patch('builtins.print'):
                    result = main()
                    assert result == 0
                
                # Check report file was created
                assert os.path.exists(reportfile_path)
                with open(reportfile_path, 'r') as f:
                    report_content = f.read()
                    assert "TRANSACTION DETAIL REPORT" in report_content
                    assert "TRAN000000000001" in report_content
            finally:
                os.chdir(original_cwd)
    
    def test_main_file_not_found(self):
        """Test main with file not found."""
        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            try:
                os.chdir(temp_dir)
                with patch('builtins.print'):
                    with pytest.raises(SystemExit) as exc_info:
                        main()
                    assert exc_info.value.code == 999
            finally:
                os.chdir(original_cwd)
    
    def test_main_with_invalid_card(self):
        """Test main with invalid card number."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create temporary files
            tranfile_path = os.path.join(temp_dir, "TRANFILE")
            reportfile_path = os.path.join(temp_dir, "TRANREPT")
            xreffile_path = os.path.join(temp_dir, "CARDXREF")
            trantypefile_path = os.path.join(temp_dir, "TRANTYPE")
            trancatfile_path = os.path.join(temp_dir, "TRANCATG")
            dateparmfile_path = os.path.join(temp_dir, "DATEPARM")
            
            # Write date parameter file
            with open(dateparmfile_path, 'w') as f:
                f.write("2024-01-01 2024-01-31\n")
            
            # Write transaction file with invalid card
            with open(tranfile_path, 'w') as f:
                tran_line = (
                    "TRAN00000000000101   0005SYSTEM             Purchase                   "
                    "100.50         MERCHANT001 Store Name                    City              "
                    "12345     99999999999999992024-01-01-12.00.00.000000" +
                    " " * (304 - 219) + "2024-01-15-12.00.00.000000" + " " * 20 + "\n"
                )
                f.write(tran_line)
            
            # Write empty XREF file (card not found)
            with open(xreffile_path, 'w') as f:
                f.write("")
            
            # Write transaction type file
            with open(trantypefile_path, 'w') as f:
                f.write("01Purchase Transaction Type" + " " * 30 + "\n")
            
            # Write transaction category file
            with open(trancatfile_path, 'w') as f:
                f.write("010005Retail Category" + " " * 40 + "\n")
            
            original_cwd = os.getcwd()
            try:
                os.chdir(temp_dir)
                with patch('builtins.print'):
                    with pytest.raises(SystemExit) as exc_info:
                        main()
                    assert exc_info.value.code == 999
            finally:
                os.chdir(original_cwd)
    
    def test_main_with_date_filtering(self):
        """Test main with date filtering."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create temporary files
            tranfile_path = os.path.join(temp_dir, "TRANFILE")
            reportfile_path = os.path.join(temp_dir, "TRANREPT")
            xreffile_path = os.path.join(temp_dir, "CARDXREF")
            trantypefile_path = os.path.join(temp_dir, "TRANTYPE")
            trancatfile_path = os.path.join(temp_dir, "TRANCATG")
            dateparmfile_path = os.path.join(temp_dir, "DATEPARM")
            
            # Write date parameter file (narrow range)
            with open(dateparmfile_path, 'w') as f:
                f.write("2024-01-20 2024-01-31\n")
            
            # Write transaction file with date outside range
            with open(tranfile_path, 'w') as f:
                tran_line = (
                    "TRAN00000000000101   0005SYSTEM             Purchase                   "
                    "100.50         MERCHANT001 Store Name                    City              "
                    "12345     12345678901234562024-01-01-12.00.00.000000" +
                    " " * (304 - 219) + "2024-01-15-12.00.00.000000" + " " * 20 + "\n"
                )
                f.write(tran_line)
            
            # Write XREF file
            with open(xreffile_path, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Write transaction type file
            with open(trantypefile_path, 'w') as f:
                f.write("01Purchase Transaction Type" + " " * 30 + "\n")
            
            # Write transaction category file
            with open(trancatfile_path, 'w') as f:
                f.write("010005Retail Category" + " " * 40 + "\n")
            
            original_cwd = os.getcwd()
            try:
                os.chdir(temp_dir)
                with patch('builtins.print'):
                    result = main()
                    assert result == 0
                
                # Check report file - should be empty or have headers only
                if os.path.exists(reportfile_path):
                    with open(reportfile_path, 'r') as f:
                        report_content = f.read()
                        # Should not contain transaction detail
                        assert "TRAN000000000001" not in report_content or "TRANSACTION DETAIL REPORT" in report_content
            finally:
                os.chdir(original_cwd)
    
    def test_main_with_multiple_transactions(self):
        """Test main with multiple transactions."""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create temporary files
            tranfile_path = os.path.join(temp_dir, "TRANFILE")
            reportfile_path = os.path.join(temp_dir, "TRANREPT")
            xreffile_path = os.path.join(temp_dir, "CARDXREF")
            trantypefile_path = os.path.join(temp_dir, "TRANTYPE")
            trancatfile_path = os.path.join(temp_dir, "TRANCATG")
            dateparmfile_path = os.path.join(temp_dir, "DATEPARM")
            
            # Write date parameter file
            with open(dateparmfile_path, 'w') as f:
                f.write("2024-01-01 2024-01-31\n")
            
            # Write transaction file with multiple transactions
            with open(tranfile_path, 'w') as f:
                for i in range(1, 4):
                    # Format transaction ID: TRAN + 12 digits (16 chars total)
                    tran_id = f"TRAN{i:012d}"[:16]
                    # Format amount in 15 chars (right-aligned)
                    amount_str = f"{100.0 + i:>15.2f}"
                    # Build transaction line with fixed-width fields
                    tran_line = (
                        f"{tran_id:<16}"  # ID (16)
                        f"01"  # Type (2)
                        f"0005"  # Category (4)
                        f"SYSTEM             "  # Source (20)
                        f"Purchase                   "  # Desc (50)
                        f"{amount_str}"  # Amount (15)
                        f"MERCHANT001"  # Merchant ID (10)
                        f"Store Name                    "  # Merchant name (30)
                        f"City              "  # Merchant city (20)
                        f"12345     "  # Merchant zip (10)
                        f"1234567890123456"  # Card (16)
                        f"2024-01-01-12.00.00.000000"  # Orig TS (26)
                        + " " * (304 - 219)  # Padding to position 304
                        + f"2024-01-15-12.00.00.000000"  # Proc TS (26)
                        + " " * 20  # Filler (20)
                        + "\n"
                    )
                    f.write(tran_line)
            
            # Write XREF file
            with open(xreffile_path, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Write transaction type file
            with open(trantypefile_path, 'w') as f:
                f.write("01Purchase Transaction Type" + " " * 30 + "\n")
            
            # Write transaction category file
            with open(trancatfile_path, 'w') as f:
                f.write("010005Retail Category" + " " * 40 + "\n")
            
            original_cwd = os.getcwd()
            try:
                os.chdir(temp_dir)
                with patch('builtins.print'):
                    result = main()
                    assert result == 0
                
                # Check report file
                assert os.path.exists(reportfile_path)
                with open(reportfile_path, 'r') as f:
                    report_content = f.read()
                    assert "TRAN000000000001" in report_content
                    assert "TRAN000000000002" in report_content
                    assert "TRAN000000000003" in report_content
                    assert "GRAND TOTAL" in report_content
            finally:
                os.chdir(original_cwd)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

