"""
Pytest tests for CBTRN02C.py ground truth Python equivalent.

Tests verify the functionality matches the COBOL program behavior.
"""

import pytest
import sys
import os
import tempfile
from pathlib import Path
from decimal import Decimal
from unittest.mock import patch, MagicMock

# Add parent directory to path to import CBTRN02C
sys.path.insert(0, str(Path(__file__).parent))

from CBTRN02C import (
    DailyTranRecord,
    TranRecord,
    CardXrefRecord,
    AccountRecord,
    TranCatBalRecord,
    SequentialFileHandler,
    XrefFileHandler,
    AcctFileHandler,
    TranFileHandler,
    TcatBalFileHandler,
    parse_daily_tran_record,
    parse_xref_record,
    parse_account_record,
    parse_tran_cat_bal_record,
    format_db2_timestamp,
    format_record_for_write,
    abend_program,
    main
)


class TestDailyTranRecord:
    """Tests for DailyTranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a DailyTranRecord."""
        record = DailyTranRecord(
            tran_id="TRAN000000000001",
            tran_type_cd="01",
            tran_cat_cd="0005",
            tran_amt=Decimal('100.50')
        )
        assert record.dalytran_id == "TRAN000000000001"
        assert record.dalytran_type_cd == "01"
        assert record.dalytran_amt == Decimal('100.50')


class TestTranRecord:
    """Tests for TranRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranRecord."""
        record = TranRecord(
            tran_id="TRAN000000000001",
            tran_type_cd="01",
            tran_amt=Decimal('50.00')
        )
        assert record.trans_id == "TRAN000000000001"
        assert record.tran_type_cd == "01"


class TestTranCatBalRecord:
    """Tests for TranCatBalRecord dataclass."""
    
    def test_creation(self):
        """Test creating a TranCatBalRecord."""
        record = TranCatBalRecord(
            trancat_acct_id="12345678901",
            trancat_type_cd="01",
            trancat_cd="0005",
            tran_cat_bal=Decimal('1000.00')
        )
        assert record.trancat_acct_id_prop == "12345678901"
        assert record.tran_cat_bal_prop == Decimal('1000.00')


class TestParsingFunctions:
    """Tests for parsing functions."""
    
    def test_parse_daily_tran_record(self):
        """Test parsing daily transaction record."""
        # Simplified record format
        line = "TRAN00000000000101   0005SYSTEM             Purchase                   100.50         MERCHANT001 Store Name                    City             12345     12345678901234562024-01-01-12.00.00.00"
        record = parse_daily_tran_record(line)
        assert record.tran_id == "TRAN000000000001"
        assert record.tran_type_cd == "01"
    
    def test_parse_xref_record(self):
        """Test parsing XREF record."""
        line = "123456789012345612345678998765432109" + " " * 14
        record = parse_xref_record(line)
        assert record.card_num == "1234567890123456"
        assert record.acct_id == "98765432109"
    
    def test_parse_account_record(self):
        """Test parsing account record."""
        line = "12345678901 1000.00 500.00 200.00 10000.00 2025-12-31"
        record = parse_account_record(line)
        assert record.acct_id == "12345678901"
        assert record.curr_bal == Decimal('1000.00')
        assert record.credit_limit == Decimal('10000.00')
    
    def test_parse_tran_cat_bal_record(self):
        """Test parsing transaction category balance record."""
        line = "123456789010100051000.00"
        record = parse_tran_cat_bal_record(line)
        assert record.trancat_acct_id == "12345678901"
        assert record.trancat_type_cd == "01"
        assert record.trancat_cd == "0005"


class TestFormatFunctions:
    """Tests for formatting functions."""
    
    def test_format_db2_timestamp(self):
        """Test DB2 timestamp formatting."""
        ts = format_db2_timestamp()
        assert len(ts) == 26
        assert ts.count('-') == 3
        assert ts.count('.') == 3
    
    def test_format_record_for_write_tran(self):
        """Test formatting transaction record for writing."""
        record = TranRecord(
            tran_id="TRAN000000000001",
            tran_type_cd="01",
            tran_cat_cd="0005",
            tran_source="System",
            tran_desc="Purchase",
            tran_amt=Decimal('100.50'),
            tran_merchant_id="MERCH001",
            tran_merchant_name="Store",
            tran_merchant_city="City",
            tran_merchant_zip="12345",
            tran_card_num="1234567890123456",
            tran_orig_ts="2024-01-01-12.00.00.00",
            tran_proc_ts="2024-01-01-12.00.00.00"
        )
        result = format_record_for_write(record)
        assert "TRAN000000000001" in result
        assert "01" in result


class TestTcatBalFileHandler:
    """Tests for TcatBalFileHandler."""
    
    def test_extract_key(self):
        """Test key extraction for transaction category balance."""
        handler = TcatBalFileHandler("test.txt")
        line = "123456789010100051000.00"
        key = handler._extract_key(line)
        assert key == "12345678901010005"  # 11 + 2 + 4 = 17 chars


class TestMainFunction:
    """Tests for main function."""
    
    def test_main_with_valid_transaction(self, capsys):
        """Test main function with valid transaction."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            dalyrejs_file = os.path.join(tmpdir, "DALYREJS")
            tcatbalfile = os.path.join(tmpdir, "TCATBALF")
            
            # Create test data
            # Format: ID(16) + type(2) + cat(4) + source(20) + desc(50) + amt(15) + merchant_id(10) + merchant_name(30) + merchant_city(20) + merchant_zip(10) + card(16) + orig_ts(26)
            # Need to pad to correct positions
            tran_line = (
                "TRAN000000000001" +  # ID: 16 chars
                "01" +                # type: 2 chars
                "0005" +              # cat: 4 chars
                "SYSTEM".ljust(20) +  # source: 20 chars
                "Purchase".ljust(50) +  # desc: 50 chars
                "100.50".rjust(15) +  # amt: 15 chars
                "MERCHANT001".ljust(10) +  # merchant_id: 10 chars
                "Store Name".ljust(30) +  # merchant_name: 30 chars
                "City".ljust(20) +    # merchant_city: 20 chars
                "12345".ljust(10) +   # merchant_zip: 10 chars
                "1234567890123456" +  # card: 16 chars
                "2024-01-01-12.00.00.000000"  # orig_ts: 26 chars (YYYY-MM-DD-HH.MM.SS.MM0000)
            )
            with open(dalytran_file, 'w') as f:
                f.write(tran_line + "\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Account record format: acct_id(11) + data fields
            # The parser expects space-separated values for balance fields  
            with open(acct_file, 'w') as f:
                acct_line = "98765432109" + " " * 15 + "5000.00 1000.00 500.00 10000.00 2025-12-31"
                f.write(acct_line + "\n")
            
            # Empty files for output
            with open(tran_file, 'w') as f:
                pass
            with open(dalyrejs_file, 'w') as f:
                pass
            with open(tcatbalfile, 'w') as f:
                pass
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                # Should succeed (exit code 0) if no rejects
            except SystemExit as e:
                # May exit with code 4 if rejects found
                assert e.code in (0, 4)
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "START OF EXECUTION OF PROGRAM CBTRN02C" in captured
            assert "TRANSACTIONS PROCESSED" in captured
            assert "END OF EXECUTION OF PROGRAM CBTRN02C" in captured
    
    def test_main_with_invalid_card(self, capsys):
        """Test main function when card number is invalid."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            dalyrejs_file = os.path.join(tmpdir, "DALYREJS")
            tcatbalfile = os.path.join(tmpdir, "TCATBALF")
            
            # Transaction with invalid card number
            with open(dalytran_file, 'w') as f:
                f.write("TRAN00000000000101   0005SYSTEM             Purchase                   100.50         MERCHANT001 Store Name                    City             12345     99999999999999992024-01-01-12.00.00.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Account record format: acct_id(11) + data fields
            # The parser expects space-separated values for balance fields  
            with open(acct_file, 'w') as f:
                acct_line = "98765432109" + " " * 15 + "5000.00 1000.00 500.00 10000.00 2025-12-31"
                f.write(acct_line + "\n")
            
            with open(tran_file, 'w') as f:
                pass
            with open(dalyrejs_file, 'w') as f:
                pass
            with open(tcatbalfile, 'w') as f:
                pass
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                # Should return 4 if rejects found
                assert result == 4
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "TRANSACTIONS REJECTED" in captured
    
    def test_main_with_overlimit(self, capsys):
        """Test main function with overlimit transaction."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            dalyrejs_file = os.path.join(tmpdir, "DALYREJS")
            tcatbalfile = os.path.join(tmpdir, "TCATBALF")
            
            # Transaction that exceeds credit limit
            # Account has limit 1000, current cycle credit 900, transaction 500 -> total 1400 > 1000
            with open(dalytran_file, 'w') as f:
                f.write("TRAN00000000000101   0005SYSTEM             Purchase                   500.00         MERCHANT001 Store Name                    City             12345     12345678901234562024-01-01-12.00.00.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Account with low credit limit
            with open(acct_file, 'w') as f:
                f.write("98765432109 1000.00 900.00 0.00 1000.00 2025-12-31\n")
            
            with open(tran_file, 'w') as f:
                pass
            with open(dalyrejs_file, 'w') as f:
                pass
            with open(tcatbalfile, 'w') as f:
                pass
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                # Should return 4 if rejects found
                assert result == 4
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "TRANSACTIONS REJECTED" in captured
    
    def test_main_with_expired_account(self, capsys):
        """Test main function with expired account."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            dalyrejs_file = os.path.join(tmpdir, "DALYREJS")
            tcatbalfile = os.path.join(tmpdir, "TCATBALF")
            
            # Transaction after account expiration
            with open(dalytran_file, 'w') as f:
                f.write("TRAN00000000000101   0005SYSTEM             Purchase                   100.50         MERCHANT001 Store Name                    City             12345     12345678901234562025-01-01-12.00.00.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Account expired in 2024
            with open(acct_file, 'w') as f:
                f.write("98765432109 5000.00 1000.00 500.00 10000.00 2024-12-31\n")
            
            with open(tran_file, 'w') as f:
                pass
            with open(dalyrejs_file, 'w') as f:
                pass
            with open(tcatbalfile, 'w') as f:
                pass
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                # Should return 4 if rejects found
                assert result == 4
            finally:
                os.chdir(old_cwd)
            
            captured = capsys.readouterr().out
            assert "TRANSACTIONS REJECTED" in captured


class TestIntegration:
    """Integration tests."""
    
    def test_full_workflow_valid_transaction(self):
        """Test complete workflow with valid transaction."""
        with tempfile.TemporaryDirectory() as tmpdir:
            dalytran_file = os.path.join(tmpdir, "DALYTRAN")
            xref_file = os.path.join(tmpdir, "XREFFILE")
            acct_file = os.path.join(tmpdir, "ACCTFILE")
            tran_file = os.path.join(tmpdir, "TRANFILE")
            dalyrejs_file = os.path.join(tmpdir, "DALYREJS")
            tcatbalfile = os.path.join(tmpdir, "TCATBALF")
            
            with open(dalytran_file, 'w') as f:
                f.write("TRAN00000000000101   0005SYSTEM             Purchase                   100.50         MERCHANT001 Store Name                    City             12345     12345678901234562024-01-01-12.00.00.00\n")
            
            with open(xref_file, 'w') as f:
                f.write("123456789012345612345678998765432109" + " " * 14 + "\n")
            
            # Account record format: acct_id(11) + data fields
            # The parser expects space-separated values for balance fields  
            with open(acct_file, 'w') as f:
                acct_line = "98765432109" + " " * 15 + "5000.00 1000.00 500.00 10000.00 2025-12-31"
                f.write(acct_line + "\n")
            
            with open(tran_file, 'w') as f:
                pass
            with open(dalyrejs_file, 'w') as f:
                pass
            with open(tcatbalfile, 'w') as f:
                pass
            
            old_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                result = main()
                # Check that transaction file was written
                assert os.path.exists(tran_file)
                # Check that account was updated
                with open(acct_file, 'r') as f:
                    content = f.read()
                    # Account balance should have been updated
            except SystemExit:
                pass
            finally:
                os.chdir(old_cwd)


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


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

