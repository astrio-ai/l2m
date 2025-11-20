"""
Test suite for COACTUPC.py

Tests for COBOL to Python translation of COACTUPC.cbl
"""

import pytest
import os
import tempfile
from decimal import Decimal
from unittest.mock import patch
import sys
sys.path.insert(0, 'data/aws-samples')

from COACTUPC import (
    CardXrefRecord,
    AccountRecord,
    CustomerRecord,
    XrefFileHandler,
    AcctFileHandler,
    CustFileHandler,
    parse_xref_record,
    parse_account_record,
    parse_customer_record,
    format_account_record,
    format_customer_record,
    edit_yes_no,
    edit_signed_number,
    edit_date_ccyymmdd,
    edit_alpha_required,
    edit_ssn,
    edit_phone_number,
    edit_fico_score,
    edit_account_id,
    read_account_by_acct_id,
    update_account_and_customer,
    records_match
)


# ============================================================================
# Test Dataclasses
# ============================================================================

class TestCardXrefRecord:
    """Test CardXrefRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CardXrefRecord."""
        record = CardXrefRecord(
            card_num="1234567890123456",
            cust_id="123456789",
            acct_id="12345678901"
        )
        assert record.card_num == "1234567890123456"
        assert record.cust_id == "123456789"
        assert record.acct_id == "12345678901"
    
    def test_properties(self):
        """Test CardXrefRecord properties."""
        record = CardXrefRecord(
            card_num="1234567890123456",
            cust_id="123456789",
            acct_id="12345678901"
        )
        assert record.xref_card_num == "1234567890123456"
        assert record.xref_cust_id == "123456789"
        assert record.xref_acct_id == "12345678901"


class TestAccountRecord:
    """Test AccountRecord dataclass."""
    
    def test_creation(self):
        """Test creating an AccountRecord."""
        record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00'),
            cash_credit_limit=Decimal('1000.00'),
            open_date="2020-01-15",
            expiration_date="2025-01-15",
            reissue_date="2024-01-15",
            curr_cyc_credit=Decimal('500.00'),
            curr_cyc_debit=Decimal('200.00'),
            group_id="GROUP001"
        )
        assert record.acct_id == "12345678901"
        assert record.active_status == "Y"
        assert record.curr_bal == Decimal('1000.50')
        assert record.credit_limit == Decimal('5000.00')
    
    def test_properties(self):
        """Test AccountRecord properties."""
        record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50')
        )
        assert record.acct_active_status == "Y"
        assert record.acct_curr_bal == Decimal('1000.50')


class TestCustomerRecord:
    """Test CustomerRecord dataclass."""
    
    def test_creation(self):
        """Test creating a CustomerRecord."""
        record = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe",
            addr_line_1="123 Main St",
            addr_state_cd="NY",
            addr_zip="10001",
            phone_num_1="(555)123-4567",
            ssn="123456789",
            fico_credit_score=750
        )
        assert record.cust_id == "123456789"
        assert record.first_name == "John"
        assert record.last_name == "Doe"
        assert record.fico_credit_score == 750


# ============================================================================
# Test File Handlers
# ============================================================================

class TestXrefFileHandler:
    """Test XrefFileHandler."""
    
    def test_open_file(self):
        """Test opening XREF file."""
        with tempfile.TemporaryDirectory() as temp_dir:
            xref_file_path = os.path.join(temp_dir, "CARDXREF")
            with open(xref_file_path, 'w') as f:
                f.write("123456789012345612345678912345678901\n")
            
            handler = XrefFileHandler(xref_file_path)
            result = handler.open_file("r")
            assert result == 0
    
    def test_read_record_by_card(self):
        """Test reading record by card number."""
        with tempfile.TemporaryDirectory() as temp_dir:
            xref_file_path = os.path.join(temp_dir, "CARDXREF")
            with open(xref_file_path, 'w') as f:
                f.write("123456789012345612345678912345678901\n")
            
            handler = XrefFileHandler(xref_file_path)
            handler.open_file("r")
            line, result = handler.read_record("1234567890123456")
            assert result == 0
            assert line is not None
            assert line.startswith("1234567890123456")
    
    def test_read_record_by_acct_id(self):
        """Test reading record by account ID (alternate index)."""
        with tempfile.TemporaryDirectory() as temp_dir:
            xref_file_path = os.path.join(temp_dir, "CARDXREF")
            with open(xref_file_path, 'w') as f:
                f.write("123456789012345612345678912345678901\n")
            
            handler = XrefFileHandler(xref_file_path)
            handler.open_file("r")
            line, result = handler.read_record("12345678901", use_alt_index=True)
            assert result == 0
            assert line is not None


class TestAcctFileHandler:
    """Test AcctFileHandler."""
    
    def test_open_file(self):
        """Test opening account file."""
        with tempfile.TemporaryDirectory() as temp_dir:
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            with open(acct_file_path, 'w') as f:
                acct_line = (
                    "12345678901Y" +
                    " " * 12 +  # curr_bal
                    " " * 12 +  # credit_limit
                    " " * 12 +  # cash_credit_limit
                    "2020-01-15" +  # open_date
                    "2025-01-15" +  # expiration_date
                    "2024-01-15" +  # reissue_date
                    " " * 12 +  # curr_cyc_credit
                    " " * 12 +  # curr_cyc_debit
                    "GROUP001 " +  # group_id
                    " " * 188  # filler
                )
                f.write(acct_line + '\n')
            
            handler = AcctFileHandler(acct_file_path)
            result = handler.open_file("r")
            assert result == 0
    
    def test_read_record(self):
        """Test reading account record."""
        with tempfile.TemporaryDirectory() as temp_dir:
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            with open(acct_file_path, 'w') as f:
                acct_line = (
                    "12345678901Y" +
                    " " * 12 +
                    " " * 12 +
                    " " * 12 +
                    "2020-01-15" +
                    "2025-01-15" +
                    "2024-01-15" +
                    " " * 12 +
                    " " * 12 +
                    "GROUP001 " +
                    " " * 188
                )
                f.write(acct_line + '\n')
            
            handler = AcctFileHandler(acct_file_path)
            handler.open_file("r")
            line, result = handler.read_record("12345678901")
            assert result == 0
            assert line is not None
            assert line.startswith("12345678901")


class TestCustFileHandler:
    """Test CustFileHandler."""
    
    def test_open_file(self):
        """Test opening customer file."""
        with tempfile.TemporaryDirectory() as temp_dir:
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            with open(cust_file_path, 'w') as f:
                cust_line = (
                    "123456789" +  # cust_id
                    "John" + " " * 21 +  # first_name
                    " " * 25 +  # middle_name
                    "Doe" + " " * 22 +  # last_name
                    "123 Main St" + " " * 39 +  # addr_line_1
                    " " * 50 +  # addr_line_2
                    "New York" + " " * 42 +  # addr_line_3
                    "NY" +  # state
                    "USA" +  # country
                    "10001" + " " * 5 +  # zip
                    "(555)123-4567" + " " * 2 +  # phone_num_1
                    " " * 15 +  # phone_num_2
                    "123456789" +  # ssn
                    " " * 20 +  # govt_issued_id
                    "1990-01-15" +  # dob
                    " " * 10 +  # eft_account_id
                    "Y" +  # pri_card_ind
                    "750" +  # fico_score
                    " " * 169  # filler
                )
                f.write(cust_line + '\n')
            
            handler = CustFileHandler(cust_file_path)
            result = handler.open_file("r")
            assert result == 0
    
    def test_read_record(self):
        """Test reading customer record."""
        with tempfile.TemporaryDirectory() as temp_dir:
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            with open(cust_file_path, 'w') as f:
                cust_line = (
                    "123456789" +
                    "John" + " " * 21 +
                    " " * 25 +
                    "Doe" + " " * 22 +
                    "123 Main St" + " " * 39 +
                    " " * 50 +
                    "New York" + " " * 42 +
                    "NY" +
                    "USA" +
                    "10001" + " " * 5 +
                    "(555)123-4567" + " " * 2 +
                    " " * 15 +
                    "123456789" +
                    " " * 20 +
                    "1990-01-15" +
                    " " * 10 +
                    "Y" +
                    "750" +
                    " " * 169
                )
                f.write(cust_line + '\n')
            
            handler = CustFileHandler(cust_file_path)
            handler.open_file("r")
            line, result = handler.read_record("123456789")
            assert result == 0
            assert line is not None
            assert line.startswith("123456789")


# ============================================================================
# Test Parsing Functions
# ============================================================================

class TestParseXrefRecord:
    """Test parse_xref_record function."""
    
    def test_parse_valid_record(self):
        """Test parsing a valid XREF record."""
        line = "123456789012345612345678912345678901" + " " * 14
        record = parse_xref_record(line)
        assert record.card_num == "1234567890123456"
        assert record.cust_id == "123456789"
        assert record.acct_id == "12345678901"
    
    def test_parse_short_line(self):
        """Test parsing a short line."""
        line = "1234567890123456"
        record = parse_xref_record(line)
        # Short line should still extract what's available
        # Card num is 16 chars, so it should be extracted
        assert len(record.card_num) == 16
        assert record.card_num == "1234567890123456"
        assert record.cust_id == ""  # Not available in short line
        assert record.acct_id == ""  # Not available in short line


class TestParseAccountRecord:
    """Test parse_account_record function."""
    
    def test_parse_valid_record(self):
        """Test parsing a valid account record."""
        acct_line = (
            "12345678901" +  # acct_id
            "Y" +  # active_status
            "     1000.50" +  # curr_bal (12 chars)
            "     5000.00" +  # credit_limit
            "     1000.00" +  # cash_credit_limit
            "2020-01-15" +  # open_date
            "2025-01-15" +  # expiration_date
            "2024-01-15" +  # reissue_date
            "      500.00" +  # curr_cyc_credit
            "      200.00" +  # curr_cyc_debit
            "GROUP001 " +  # group_id
            " " * 188  # filler
        )
        record = parse_account_record(acct_line)
        assert record.acct_id == "12345678901"
        assert record.active_status == "Y"
        assert record.open_date == "2020-01-15"
        assert record.group_id.strip() == "GROUP001"
    
    def test_parse_short_line(self):
        """Test parsing a short line."""
        line = "12345678901Y"
        record = parse_account_record(line)
        # Short line should still extract what's available
        if len(line) >= 11:
            assert record.acct_id == "12345678901"
        if len(line) >= 12:
            assert record.active_status == "Y"


class TestParseCustomerRecord:
    """Test parse_customer_record function."""
    
    def test_parse_valid_record(self):
        """Test parsing a valid customer record."""
        cust_line = (
            "123456789" +
            "John" + " " * 21 +
            " " * 25 +
            "Doe" + " " * 22 +
            "123 Main St" + " " * 39 +
            " " * 50 +
            "New York" + " " * 42 +
            "NY" +
            "USA" +
            "10001" + " " * 5 +
            "(555)123-4567" + " " * 2 +
            " " * 15 +
            "123456789" +
            " " * 20 +
            "1990-01-15" +
            " " * 10 +
            "Y" +
            "750" +
            " " * 169
        )
        record = parse_customer_record(cust_line)
        assert record.cust_id == "123456789"
        assert record.first_name.strip() == "John"
        assert record.last_name.strip() == "Doe"
        assert record.addr_state_cd == "NY"
        assert record.fico_credit_score == 750


# ============================================================================
# Test Formatting Functions
# ============================================================================

class TestFormatAccountRecord:
    """Test format_account_record function."""
    
    def test_format_valid_record(self):
        """Test formatting a valid account record."""
        record = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00'),
            cash_credit_limit=Decimal('1000.00'),
            open_date="2020-01-15",
            expiration_date="2025-01-15",
            reissue_date="2024-01-15",
            curr_cyc_credit=Decimal('500.00'),
            curr_cyc_debit=Decimal('200.00'),
            group_id="GROUP001"
        )
        formatted = format_account_record(record)
        assert len(formatted) == 300
        assert formatted.startswith("12345678901")
        assert formatted[11] == "Y"


class TestFormatCustomerRecord:
    """Test format_customer_record function."""
    
    def test_format_valid_record(self):
        """Test formatting a valid customer record."""
        record = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe",
            addr_line_1="123 Main St",
            addr_state_cd="NY",
            addr_zip="10001",
            phone_num_1="(555)123-4567",
            ssn="123456789",
            fico_credit_score=750
        )
        formatted = format_customer_record(record)
        assert len(formatted) == 500
        assert formatted.startswith("123456789")


# ============================================================================
# Test Validation Functions
# ============================================================================

class TestEditYesNo:
    """Test edit_yes_no function."""
    
    def test_valid_y(self):
        """Test valid Y value."""
        valid, error = edit_yes_no("Y", "Status")
        assert valid is True
        assert error is None
    
    def test_valid_n(self):
        """Test valid N value."""
        valid, error = edit_yes_no("N", "Status")
        assert valid is True
        assert error is None
    
    def test_invalid_value(self):
        """Test invalid value."""
        valid, error = edit_yes_no("X", "Status")
        assert valid is False
        assert error is not None
    
    def test_blank_value(self):
        """Test blank value."""
        valid, error = edit_yes_no("", "Status")
        assert valid is False
        assert error is not None


class TestEditSignedNumber:
    """Test edit_signed_number function."""
    
    def test_valid_positive(self):
        """Test valid positive number."""
        valid, error, value = edit_signed_number("1000.50", "Amount")
        assert valid is True
        assert error is None
        assert value == Decimal('1000.50')
    
    def test_valid_negative(self):
        """Test valid negative number."""
        valid, error, value = edit_signed_number("-100.25", "Amount")
        assert valid is True
        assert error is None
        assert value == Decimal('-100.25')
    
    def test_valid_with_comma(self):
        """Test valid number with comma."""
        valid, error, value = edit_signed_number("1,000.50", "Amount")
        assert valid is True
        assert error is None
        assert value == Decimal('1000.50')
    
    def test_blank_value(self):
        """Test blank value."""
        valid, error, value = edit_signed_number("", "Amount")
        assert valid is False
        assert error is not None
        assert value is None
    
    def test_invalid_value(self):
        """Test invalid value."""
        valid, error, value = edit_signed_number("ABC", "Amount")
        assert valid is False
        assert error is not None
        assert value is None


class TestEditDateCcyymmdd:
    """Test edit_date_ccyymmdd function."""
    
    def test_valid_date(self):
        """Test valid date."""
        valid, error, date_str = edit_date_ccyymmdd("2024", "01", "15", "Open Date")
        assert valid is True
        assert error is None
        assert date_str == "2024-01-15"
    
    def test_invalid_month(self):
        """Test invalid month."""
        valid, error, date_str = edit_date_ccyymmdd("2024", "13", "15", "Open Date")
        assert valid is False
        assert error is not None
    
    def test_invalid_day(self):
        """Test invalid day."""
        valid, error, date_str = edit_date_ccyymmdd("2024", "01", "32", "Open Date")
        assert valid is False
        assert error is not None
    
    def test_blank_year(self):
        """Test blank year."""
        valid, error, date_str = edit_date_ccyymmdd("", "01", "15", "Open Date")
        assert valid is False
        assert error is not None


class TestEditAlphaRequired:
    """Test edit_alpha_required function."""
    
    def test_valid_alpha(self):
        """Test valid alphabetic value."""
        valid, error = edit_alpha_required("John Doe", "Name")
        assert valid is True
        assert error is None
    
    def test_invalid_numeric(self):
        """Test invalid numeric value."""
        valid, error = edit_alpha_required("John123", "Name")
        assert valid is False
        assert error is not None
    
    def test_blank_value(self):
        """Test blank value."""
        valid, error = edit_alpha_required("", "Name")
        assert valid is False
        assert error is not None


class TestEditSSN:
    """Test edit_ssn function."""
    
    def test_valid_ssn(self):
        """Test valid SSN."""
        valid, error, ssn = edit_ssn("123", "45", "6789")
        assert valid is True
        assert error is None
        assert ssn == "123456789"
    
    def test_invalid_part1_zero(self):
        """Test invalid part 1 (000)."""
        valid, error, ssn = edit_ssn("000", "45", "6789")
        assert valid is False
        assert error is not None
    
    def test_invalid_part1_666(self):
        """Test invalid part 1 (666)."""
        valid, error, ssn = edit_ssn("666", "45", "6789")
        assert valid is False
        assert error is not None
    
    def test_invalid_part1_900(self):
        """Test invalid part 1 (900)."""
        valid, error, ssn = edit_ssn("900", "45", "6789")
        assert valid is False
        assert error is not None
    
    def test_blank_part1(self):
        """Test blank part 1."""
        valid, error, ssn = edit_ssn("", "45", "6789")
        assert valid is False
        assert error is not None


class TestEditPhoneNumber:
    """Test edit_phone_number function."""
    
    def test_valid_phone(self):
        """Test valid phone number."""
        valid, error, phone = edit_phone_number("555", "123", "4567", "Phone")
        assert valid is True
        assert error is None
        assert phone == "(555)123-4567"
    
    def test_blank_phone(self):
        """Test blank phone (optional field)."""
        valid, error, phone = edit_phone_number("", "", "", "Phone")
        assert valid is True
        assert error is None
        assert phone == ""
    
    def test_invalid_area_code(self):
        """Test invalid area code."""
        valid, error, phone = edit_phone_number("000", "123", "4567", "Phone")
        assert valid is False
        assert error is not None
    
    def test_invalid_prefix(self):
        """Test invalid prefix."""
        valid, error, phone = edit_phone_number("555", "000", "4567", "Phone")
        assert valid is False
        assert error is not None


class TestEditFicoScore:
    """Test edit_fico_score function."""
    
    def test_valid_score(self):
        """Test valid FICO score."""
        valid, error, score = edit_fico_score("750", "FICO Score")
        assert valid is True
        assert error is None
        assert score == 750
    
    def test_invalid_low_score(self):
        """Test invalid low score."""
        valid, error, score = edit_fico_score("299", "FICO Score")
        assert valid is False
        assert error is not None
    
    def test_invalid_high_score(self):
        """Test invalid high score."""
        valid, error, score = edit_fico_score("851", "FICO Score")
        assert valid is False
        assert error is not None
    
    def test_blank_score(self):
        """Test blank score."""
        valid, error, score = edit_fico_score("", "FICO Score")
        assert valid is False
        assert error is not None


class TestEditAccountId:
    """Test edit_account_id function."""
    
    def test_valid_account_id(self):
        """Test valid account ID."""
        valid, error = edit_account_id("12345678901")
        assert valid is True
        assert error is None
    
    def test_invalid_length(self):
        """Test invalid length."""
        valid, error = edit_account_id("123456789")
        assert valid is False
        assert error is not None
    
    def test_invalid_zero(self):
        """Test invalid zero account ID."""
        valid, error = edit_account_id("00000000000")
        assert valid is False
        assert error is not None
    
    def test_blank_account_id(self):
        """Test blank account ID."""
        valid, error = edit_account_id("")
        assert valid is False
        assert error is not None
    
    def test_non_numeric(self):
        """Test non-numeric account ID."""
        valid, error = edit_account_id("123456789AB")
        assert valid is False
        assert error is not None


# ============================================================================
# Test Business Logic Functions
# ============================================================================

class TestReadAccountByAcctId:
    """Test read_account_by_acct_id function."""
    
    def test_read_success(self):
        """Test successful read."""
        with tempfile.TemporaryDirectory() as temp_dir:
            xref_file_path = os.path.join(temp_dir, "CARDXREF")
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            
            # Write XREF file
            with open(xref_file_path, 'w') as f:
                f.write("123456789012345612345678912345678901" + " " * 14 + "\n")
            
            # Write account file
            with open(acct_file_path, 'w') as f:
                acct_line = (
                    "12345678901" +
                    "Y" +
                    "     1000.50" +
                    "     5000.00" +
                    "     1000.00" +
                    "2020-01-15" +
                    "2025-01-15" +
                    "2024-01-15" +
                    "      500.00" +
                    "      200.00" +
                    "GROUP001 " +
                    " " * 188
                )
                f.write(acct_line + '\n')
            
            # Write customer file
            with open(cust_file_path, 'w') as f:
                cust_line = (
                    "123456789" +
                    "John" + " " * 21 +
                    " " * 25 +
                    "Doe" + " " * 22 +
                    "123 Main St" + " " * 39 +
                    " " * 50 +
                    "New York" + " " * 42 +
                    "NY" +
                    "USA" +
                    "10001" + " " * 5 +
                    "(555)123-4567" + " " * 2 +
                    " " * 15 +
                    "123456789" +
                    " " * 20 +
                    "1990-01-15" +
                    " " * 10 +
                    "Y" +
                    "750" +
                    " " * 169
                )
                f.write(cust_line + '\n')
            
            xref_file = XrefFileHandler(xref_file_path)
            acct_file = AcctFileHandler(acct_file_path)
            cust_file = CustFileHandler(cust_file_path)
            
            xref_file.open_file("r")
            acct_file.open_file("r")
            cust_file.open_file("r")
            
            acct_record, cust_record, error = read_account_by_acct_id(
                "12345678901", xref_file, acct_file, cust_file
            )
            
            assert acct_record is not None
            assert cust_record is not None
            assert error is None
            assert acct_record.acct_id == "12345678901"
            assert cust_record.cust_id == "123456789"
    
    def test_read_account_not_found_in_xref(self):
        """Test reading account not found in XREF."""
        with tempfile.TemporaryDirectory() as temp_dir:
            xref_file_path = os.path.join(temp_dir, "CARDXREF")
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            
            # Write empty XREF file
            with open(xref_file_path, 'w') as f:
                pass
            
            xref_file = XrefFileHandler(xref_file_path)
            acct_file = AcctFileHandler(acct_file_path)
            cust_file = CustFileHandler(cust_file_path)
            
            xref_file.open_file("r")
            acct_file.open_file("r")
            cust_file.open_file("r")
            
            acct_record, cust_record, error = read_account_by_acct_id(
                "99999999999", xref_file, acct_file, cust_file
            )
            
            assert acct_record is None
            assert cust_record is None
            assert error is not None
            assert "not found in Cross ref file" in error


class TestRecordsMatch:
    """Test records_match function."""
    
    def test_matching_records(self):
        """Test matching records."""
        acct1 = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00'),
            cash_credit_limit=Decimal('1000.00'),
            open_date="2020-01-15",
            expiration_date="2025-01-15",
            reissue_date="2024-01-15",
            curr_cyc_credit=Decimal('500.00'),
            curr_cyc_debit=Decimal('200.00'),
            group_id="GROUP001"
        )
        acct2 = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50'),
            credit_limit=Decimal('5000.00'),
            cash_credit_limit=Decimal('1000.00'),
            open_date="2020-01-15",
            expiration_date="2025-01-15",
            reissue_date="2024-01-15",
            curr_cyc_credit=Decimal('500.00'),
            curr_cyc_debit=Decimal('200.00'),
            group_id="GROUP001"
        )
        cust1 = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe",
            ssn="123456789",
            fico_credit_score=750
        )
        cust2 = CustomerRecord(
            cust_id="123456789",
            first_name="John",
            last_name="Doe",
            ssn="123456789",
            fico_credit_score=750
        )
        
        assert records_match(acct1, acct2, cust1, cust2) is True
    
    def test_different_account_status(self):
        """Test different account status."""
        acct1 = AccountRecord(
            acct_id="12345678901",
            active_status="Y",
            curr_bal=Decimal('1000.50')
        )
        acct2 = AccountRecord(
            acct_id="12345678901",
            active_status="N",
            curr_bal=Decimal('1000.50')
        )
        cust1 = CustomerRecord(cust_id="123456789")
        cust2 = CustomerRecord(cust_id="123456789")
        
        assert records_match(acct1, acct2, cust1, cust2) is False
    
    def test_different_customer_name(self):
        """Test different customer name."""
        acct1 = AccountRecord(acct_id="12345678901")
        acct2 = AccountRecord(acct_id="12345678901")
        cust1 = CustomerRecord(cust_id="123456789", first_name="John")
        cust2 = CustomerRecord(cust_id="123456789", first_name="Jane")
        
        assert records_match(acct1, acct2, cust1, cust2) is False


class TestUpdateAccountAndCustomer:
    """Test update_account_and_customer function."""
    
    def test_update_success(self):
        """Test successful update."""
        with tempfile.TemporaryDirectory() as temp_dir:
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            
            # Write initial account file
            with open(acct_file_path, 'w') as f:
                acct_line = format_account_record(AccountRecord(
                    acct_id="12345678901",
                    active_status="Y",
                    curr_bal=Decimal('1000.50'),
                    credit_limit=Decimal('5000.00'),
                    group_id="GROUP001"
                ))
                f.write(acct_line + '\n')
            
            # Write initial customer file
            with open(cust_file_path, 'w') as f:
                cust_line = format_customer_record(CustomerRecord(
                    cust_id="123456789",
                    first_name="John",
                    last_name="Doe"
                ))
                f.write(cust_line + '\n')
            
            acct_file = AcctFileHandler(acct_file_path)
            cust_file = CustFileHandler(cust_file_path)
            
            acct_file.open_file("r+")
            cust_file.open_file("r+")
            
            old_acct = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1000.50'),
                credit_limit=Decimal('5000.00'),
                group_id="GROUP001"
            )
            new_acct = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1500.75'),
                credit_limit=Decimal('6000.00'),
                group_id="GROUP001"
            )
            old_cust = CustomerRecord(
                cust_id="123456789",
                first_name="John",
                last_name="Doe"
            )
            new_cust = CustomerRecord(
                cust_id="123456789",
                first_name="John",
                last_name="Smith"
            )
            
            success, error = update_account_and_customer(
                "12345678901", "123456789",
                new_acct, new_cust,
                old_acct, old_cust,
                acct_file, cust_file
            )
            
            assert success is True
            assert error is None
            
            # Verify update
            acct_file.close_file()
            cust_file.close_file()
            
            # Reopen and verify
            acct_file2 = AcctFileHandler(acct_file_path)
            cust_file2 = CustFileHandler(cust_file_path)
            acct_file2.open_file("r")
            cust_file2.open_file("r")
            
            acct_line, _ = acct_file2.read_record("12345678901")
            updated_acct = parse_account_record(acct_line)
            assert updated_acct.curr_bal == Decimal('1500.75')
            assert updated_acct.credit_limit == Decimal('6000.00')
    
    def test_update_with_record_changed(self):
        """Test update when record was changed by someone else."""
        with tempfile.TemporaryDirectory() as temp_dir:
            acct_file_path = os.path.join(temp_dir, "ACCTDAT")
            cust_file_path = os.path.join(temp_dir, "CUSTDAT")
            
            # Write initial account file with different balance
            with open(acct_file_path, 'w') as f:
                acct_line = format_account_record(AccountRecord(
                    acct_id="12345678901",
                    active_status="Y",
                    curr_bal=Decimal('2000.00'),  # Different from old_acct
                    credit_limit=Decimal('5000.00'),
                    group_id="GROUP001"
                ))
                f.write(acct_line + '\n')
            
            # Write initial customer file
            with open(cust_file_path, 'w') as f:
                cust_line = format_customer_record(CustomerRecord(
                    cust_id="123456789",
                    first_name="John",
                    last_name="Doe"
                ))
                f.write(cust_line + '\n')
            
            acct_file = AcctFileHandler(acct_file_path)
            cust_file = CustFileHandler(cust_file_path)
            
            acct_file.open_file("r+")
            cust_file.open_file("r+")
            
            old_acct = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1000.50'),  # Different from file
                credit_limit=Decimal('5000.00'),
                group_id="GROUP001"
            )
            new_acct = AccountRecord(
                acct_id="12345678901",
                active_status="Y",
                curr_bal=Decimal('1500.75'),
                credit_limit=Decimal('6000.00'),
                group_id="GROUP001"
            )
            old_cust = CustomerRecord(
                cust_id="123456789",
                first_name="John",
                last_name="Doe"
            )
            new_cust = CustomerRecord(
                cust_id="123456789",
                first_name="John",
                last_name="Smith"
            )
            
            success, error = update_account_and_customer(
                "12345678901", "123456789",
                new_acct, new_cust,
                old_acct, old_cust,
                acct_file, cust_file
            )
            
            assert success is False
            assert error is not None
            assert "changed by some one else" in error


class TestMainFunction:
    """Test main function."""
    
    def test_main(self):
        """Test main function."""
        from COACTUPC import main
        result = main()
        assert result == 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

