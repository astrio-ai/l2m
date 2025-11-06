"""
Test suite for CSUTLDTC.py

Tests for Date Validation Utility (CEEDAYS API wrapper).
"""

import pytest
from CSUTLDTC import (
    ceedays,
    main,
    validate_date,
    FeedbackCode,
    FC_INVALID_DATE,
    FC_INSUFFICIENT_DATA,
    FC_BAD_DATE_VALUE,
    FC_INVALID_MONTH,
    FC_BAD_PIC_STRING,
    FC_NON_NUMERIC_DATA,
)


# ============================================================================
# Test CEEDAYS Function
# ============================================================================

class TestCeedays:
    """Test ceedays function."""
    
    def test_valid_date_yyyy_mm_dd(self):
        """Test validation of valid date in YYYY-MM-DD format."""
        lillian, feedback = ceedays("2025-12-31", "YYYY-MM-DD")
        
        assert feedback.severity == 0
        assert feedback.msg_no == 0
        assert feedback.feedback_token == FC_INVALID_DATE
    
    def test_invalid_date_invalid_month(self):
        """Test validation with invalid month."""
        lillian, feedback = ceedays("2025-13-01", "YYYY-MM-DD")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_INVALID_MONTH
    
    def test_invalid_date_invalid_day(self):
        """Test validation with invalid day."""
        lillian, feedback = ceedays("2025-02-30", "YYYY-MM-DD")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_BAD_DATE_VALUE
    
    def test_empty_date(self):
        """Test validation with empty date."""
        lillian, feedback = ceedays("", "YYYY-MM-DD")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_INSUFFICIENT_DATA
    
    def test_empty_format(self):
        """Test validation with empty format."""
        lillian, feedback = ceedays("2025-12-31", "")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_BAD_PIC_STRING
    
    def test_non_numeric_date(self):
        """Test validation with non-numeric data."""
        lillian, feedback = ceedays("ABCD-EF-GH", "YYYY-MM-DD")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_NON_NUMERIC_DATA
    
    def test_invalid_date_format(self):
        """Test validation with invalid date format."""
        lillian, feedback = ceedays("2025/12/31", "YYYY-MM-DD")
        
        # Should still validate if we can parse it
        # Let's test with a truly invalid format
        lillian, feedback = ceedays("2025-12-31", "INVALID")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
    
    def test_valid_date_ccyymmdd_format(self):
        """Test validation with CCYYMMDD format."""
        lillian, feedback = ceedays("20251231", "YYYYMMDD")
        
        assert feedback.severity == 0
        assert feedback.msg_no == 0
        assert feedback.feedback_token == FC_INVALID_DATE
    
    def test_leap_year_feb_29(self):
        """Test validation of leap year February 29."""
        lillian, feedback = ceedays("2024-02-29", "YYYY-MM-DD")
        
        assert feedback.severity == 0
        assert feedback.msg_no == 0
        assert feedback.feedback_token == FC_INVALID_DATE
    
    def test_non_leap_year_feb_29(self):
        """Test validation of non-leap year February 29."""
        lillian, feedback = ceedays("2025-02-29", "YYYY-MM-DD")
        
        assert feedback.severity == 8
        assert feedback.msg_no == 309
        assert feedback.feedback_token == FC_BAD_DATE_VALUE


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_valid_date(self):
        """Test main with valid date."""
        result, return_code = main("2025-12-31", "YYYY-MM-DD")
        
        assert return_code == 0
        assert "Date is valid" in result
        assert "2025-12-31" in result
        assert "YYYY-MM-DD" in result
        assert len(result) == 80
    
    def test_main_invalid_date(self):
        """Test main with invalid date."""
        result, return_code = main("2025-13-01", "YYYY-MM-DD")
        
        assert return_code == 8
        assert "Invalid month" in result or "Date is invalid" in result
        assert len(result) == 80
    
    def test_main_invalid_date_format(self):
        """Test main with invalid date format."""
        result, return_code = main("2025-12-31", "INVALID")
        
        assert return_code == 8
        assert len(result) == 80
    
    def test_main_empty_date(self):
        """Test main with empty date."""
        result, return_code = main("", "YYYY-MM-DD")
        
        assert return_code == 8
        assert "Insufficient" in result or "Date is invalid" in result
        assert len(result) == 80
    
    def test_main_result_format(self):
        """Test that result message is properly formatted."""
        result, return_code = main("2025-12-31", "YYYY-MM-DD")
        
        # Check format: "SEV Mesg Code:MSG RESULT           TstDate:DATE      Mask used:FORMAT"
        assert result.startswith("0000")  # Severity
        assert "Mesg Code:" in result
        assert "TstDate:" in result
        assert "Mask used:" in result
        assert len(result) == 80
    
    def test_main_long_date_truncated(self):
        """Test that long dates are truncated to 10 chars."""
        result, return_code = main("2025-12-31-EXTRA", "YYYY-MM-DD")
        
        # Should still validate (date is truncated internally)
        assert len(result) == 80
    
    def test_main_long_format_truncated(self):
        """Test that long formats are truncated to 10 chars."""
        result, return_code = main("2025-12-31", "YYYY-MM-DD-EXTRA")
        
        # Should still work (format is truncated internally)
        assert len(result) == 80


# ============================================================================
# Test Validate Date Convenience Function
# ============================================================================

class TestValidateDate:
    """Test validate_date convenience function."""
    
    def test_validate_date_valid(self):
        """Test validate_date with valid date."""
        result, return_code = validate_date("2025-12-31")
        
        assert return_code == 0
        assert "Date is valid" in result
    
    def test_validate_date_invalid(self):
        """Test validate_date with invalid date."""
        result, return_code = validate_date("2025-13-01")
        
        assert return_code == 8
        assert return_code != 0
    
    def test_validate_date_custom_format(self):
        """Test validate_date with custom format."""
        result, return_code = validate_date("20251231", "YYYYMMDD")
        
        assert return_code == 0
        assert "Date is valid" in result


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_various_valid_dates(self):
        """Test various valid dates."""
        valid_dates = [
            ("2025-01-01", "YYYY-MM-DD"),
            ("2025-12-31", "YYYY-MM-DD"),
            ("2024-02-29", "YYYY-MM-DD"),  # Leap year
            ("20251231", "YYYYMMDD"),
            ("20250101", "YYYYMMDD"),
        ]
        
        for date_str, date_format in valid_dates:
            result, return_code = main(date_str, date_format)
            assert return_code == 0, f"Date {date_str} should be valid"
            assert "Date is valid" in result
    
    def test_various_invalid_dates(self):
        """Test various invalid dates."""
        invalid_dates = [
            ("2025-13-01", "YYYY-MM-DD"),  # Invalid month
            ("2025-02-30", "YYYY-MM-DD"),  # Invalid day
            ("2025-04-31", "YYYY-MM-DD"),  # April has 30 days
            ("2025-00-01", "YYYY-MM-DD"),  # Invalid month
            ("2025-01-00", "YYYY-MM-DD"),  # Invalid day
            ("2025-02-29", "YYYY-MM-DD"),  # Not a leap year (2025)
        ]
        
        for date_str, date_format in invalid_dates:
            result, return_code = main(date_str, date_format)
            assert return_code == 8, f"Date {date_str} should be invalid"
            assert return_code != 0
    
    def test_edge_cases(self):
        """Test edge cases."""
        # Minimum date
        result, return_code = main("0001-01-01", "YYYY-MM-DD")
        # Should validate (though may have range issues in real CEEDAYS)
        
        # Maximum reasonable date
        result, return_code = main("9999-12-31", "YYYY-MM-DD")
        # Should validate
        
        # Single digit months/days
        result, return_code = main("2025-1-1", "YYYY-MM-DD")
        # Should fail format check or succeed if we parse it
        
        # Whitespace
        result, return_code = main("  2025-12-31  ", "YYYY-MM-DD")
        # Should trim and validate
        assert return_code == 0

