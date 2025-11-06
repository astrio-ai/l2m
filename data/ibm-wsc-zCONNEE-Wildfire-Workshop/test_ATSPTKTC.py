"""
Test suite for ATSPTKTC.py

Tests for RACF Pass Ticket Generation program.
"""

import pytest
import os
from unittest.mock import patch, MagicMock
from ATSPTKTC import (
    FeedbackCode,
    IRRArea,
    IRRApplid,
    IRRIdentity,
    IRRPassticket,
    ceeenv_get,
    ceeenv_set,
    irrspk00,
    trim_trailing_spaces,
    main
)


# ============================================================================
# Test Feedback Code
# ============================================================================

class TestFeedbackCode:
    """Test FeedbackCode structure."""
    
    def test_feedback_code_creation(self):
        """Test creating a feedback code."""
        feedback = FeedbackCode()
        
        assert feedback.severity == 0
        assert feedback.msg_no == 0


# ============================================================================
# Test IRR Structures
# ============================================================================

class TestIRRStructures:
    """Test IRR structures."""
    
    def test_irr_area_creation(self):
        """Test creating IRR area."""
        irr_area = IRRArea()
        
        assert irr_area.safrc == 0
        assert irr_area.function_code == b'\x00\x03'
    
    def test_irr_applid_creation(self):
        """Test creating IRR applid."""
        applid = IRRApplid()
        applid.applid = "TESTAPP"
        applid.applid_length = 7
        
        assert applid.applid == "TESTAPP"
        assert applid.applid_length == 7
    
    def test_irr_identity_creation(self):
        """Test creating IRR identity."""
        identity = IRRIdentity()
        identity.identity = "TESTUSER"
        identity.identity_length = 8
        
        assert identity.identity == "TESTUSER"
        assert identity.identity_length == 8
    
    def test_irr_passticket_creation(self):
        """Test creating IRR pass ticket."""
        passticket = IRRPassticket()
        passticket.pass_ticket = "PASSTICK"
        passticket.pass_ticket_length = 8
        
        assert passticket.pass_ticket == "PASSTICK"
        assert passticket.pass_ticket_length == 8


# ============================================================================
# Test CEEENV Functions
# ============================================================================

class TestCEEENV:
    """Test CEEENV API functions."""
    
    def test_ceeenv_get_existing(self):
        """Test getting an existing environment variable."""
        os.environ["TEST_VAR"] = "test_value"
        
        try:
            value, length, feedback = ceeenv_get("TEST_VAR")
            
            assert value == "test_value"
            assert length == 10
            assert feedback.severity == 0
        finally:
            if "TEST_VAR" in os.environ:
                del os.environ["TEST_VAR"]
    
    def test_ceeenv_get_not_found(self):
        """Test getting a non-existent environment variable."""
        # Ensure variable doesn't exist
        if "NONEXISTENT_VAR" in os.environ:
            del os.environ["NONEXISTENT_VAR"]
        
        value, length, feedback = ceeenv_get("NONEXISTENT_VAR")
        
        assert value is None
        assert length == 0
        assert feedback.severity == 0
    
    def test_ceeenv_get_with_trailing_spaces(self):
        """Test getting environment variable with trailing spaces in name."""
        os.environ["TEST_VAR"] = "test_value"
        
        try:
            value, length, feedback = ceeenv_get("TEST_VAR   ")
            
            assert value == "test_value"
            assert length == 10
        finally:
            if "TEST_VAR" in os.environ:
                del os.environ["TEST_VAR"]
    
    def test_ceeenv_set(self):
        """Test setting an environment variable."""
        # Ensure variable doesn't exist initially
        if "TEST_SET_VAR" in os.environ:
            del os.environ["TEST_SET_VAR"]
        
        try:
            return_code, feedback = ceeenv_set("TEST_SET_VAR", "test_value")
            
            assert return_code == 0
            assert feedback.severity == 0
            assert os.environ.get("TEST_SET_VAR") == "test_value"
        finally:
            if "TEST_SET_VAR" in os.environ:
                del os.environ["TEST_SET_VAR"]
    
    def test_ceeenv_set_with_trailing_spaces(self):
        """Test setting environment variable with trailing spaces in name."""
        try:
            return_code, feedback = ceeenv_set("TEST_SET_VAR   ", "test_value")
            
            assert return_code == 0
            # Should trim trailing spaces
            assert os.environ.get("TEST_SET_VAR") == "test_value"
        finally:
            if "TEST_SET_VAR" in os.environ:
                del os.environ["TEST_SET_VAR"]


# ============================================================================
# Test RACF IRRSPK00
# ============================================================================

class TestIRRSPK00:
    """Test IRRSPK00 RACF service."""
    
    def test_irrspk00_success(self):
        """Test successful pass ticket generation."""
        irr_area = IRRArea()
        irr_passticket = IRRPassticket()
        irr_identity = IRRIdentity()
        irr_identity.identity = "TESTUSER"
        irr_identity.identity_length = 8
        irr_applid = IRRApplid()
        irr_applid.applid = "TESTAPP"
        irr_applid.applid_length = 7
        
        safrc, racfrc, racfrsn = irrspk00(
            irr_area,
            irr_passticket,
            irr_identity,
            irr_applid
        )
        
        assert safrc == 0
        assert racfrc == 0
        assert racfrsn == 0
        assert len(irr_passticket.pass_ticket) == 8
        assert irr_passticket.pass_ticket_length == 8
    
    def test_irrspk00_empty_identity(self):
        """Test with empty identity."""
        irr_area = IRRArea()
        irr_passticket = IRRPassticket()
        irr_identity = IRRIdentity()
        irr_identity.identity = ""
        irr_identity.identity_length = 0
        irr_applid = IRRApplid()
        irr_applid.applid = "TESTAPP"
        irr_applid.applid_length = 7
        
        safrc, racfrc, racfrsn = irrspk00(
            irr_area,
            irr_passticket,
            irr_identity,
            irr_applid
        )
        
        assert safrc == 8
        assert racfrc == 8
        assert racfrsn == 1
    
    def test_irrspk00_empty_applid(self):
        """Test with empty applid."""
        irr_area = IRRArea()
        irr_passticket = IRRPassticket()
        irr_identity = IRRIdentity()
        irr_identity.identity = "TESTUSER"
        irr_identity.identity_length = 8
        irr_applid = IRRApplid()
        irr_applid.applid = ""
        irr_applid.applid_length = 0
        
        safrc, racfrc, racfrsn = irrspk00(
            irr_area,
            irr_passticket,
            irr_identity,
            irr_applid
        )
        
        assert safrc == 8
        assert racfrc == 8
        assert racfrsn == 2
    
    def test_irrspk00_long_identity(self):
        """Test with long identity (should truncate to 8 chars)."""
        irr_area = IRRArea()
        irr_passticket = IRRPassticket()
        irr_identity = IRRIdentity()
        irr_identity.identity = "VERYLONGUSERNAME"
        irr_identity.identity_length = 16
        irr_applid = IRRApplid()
        irr_applid.applid = "TESTAPP"
        irr_applid.applid_length = 7
        
        safrc, racfrc, racfrsn = irrspk00(
            irr_area,
            irr_passticket,
            irr_identity,
            irr_applid
        )
        
        assert safrc == 0
        assert len(irr_passticket.pass_ticket) == 8


# ============================================================================
# Test Helper Functions
# ============================================================================

class TestHelperFunctions:
    """Test helper functions."""
    
    def test_trim_trailing_spaces(self):
        """Test trimming trailing spaces."""
        s, length = trim_trailing_spaces("test   ")
        
        assert s == "test"
        assert length == 4
    
    def test_trim_trailing_spaces_no_spaces(self):
        """Test trimming string with no trailing spaces."""
        s, length = trim_trailing_spaces("test")
        
        assert s == "test"
        assert length == 4
    
    def test_trim_trailing_spaces_all_spaces(self):
        """Test trimming string with all spaces."""
        s, length = trim_trailing_spaces("    ")
        
        assert s == ""
        assert length == 0


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_success(self):
        """Test main with successful pass ticket generation."""
        # Set up environment variables
        os.environ["BAQUSERNAME"] = "TESTUSER"
        os.environ["ATSAPPLID"] = "TESTAPP"
        
        try:
            # Clear BAQPASSWORD if it exists
            if "BAQPASSWORD" in os.environ:
                del os.environ["BAQPASSWORD"]
            
            result = main()
            
            assert result == 0
            assert "BAQPASSWORD" in os.environ
            assert len(os.environ["BAQPASSWORD"]) == 8
        finally:
            # Cleanup
            for var in ["BAQUSERNAME", "ATSAPPLID", "BAQPASSWORD"]:
                if var in os.environ:
                    del os.environ[var]
    
    def test_main_missing_baqusername(self):
        """Test main with missing BAQUSERNAME."""
        # Ensure BAQUSERNAME doesn't exist
        if "BAQUSERNAME" in os.environ:
            del os.environ["BAQUSERNAME"]
        if "ATSAPPLID" in os.environ:
            del os.environ["ATSAPPLID"]
        
        result = main()
        
        assert result == 1
    
    def test_main_missing_atsapplid(self):
        """Test main with missing ATSAPPLID."""
        os.environ["BAQUSERNAME"] = "TESTUSER"
        
        try:
            # Ensure ATSAPPLID doesn't exist
            if "ATSAPPLID" in os.environ:
                del os.environ["ATSAPPLID"]
            
            result = main()
            
            assert result == 1
        finally:
            if "BAQUSERNAME" in os.environ:
                del os.environ["BAQUSERNAME"]
    
    def test_main_with_spaces(self):
        """Test main with environment variables containing spaces."""
        os.environ["BAQUSERNAME"] = "TESTUSER   "
        os.environ["ATSAPPLID"] = "TESTAPP   "
        
        try:
            if "BAQPASSWORD" in os.environ:
                del os.environ["BAQPASSWORD"]
            
            result = main()
            
            assert result == 0
            assert "BAQPASSWORD" in os.environ
        finally:
            for var in ["BAQUSERNAME", "ATSAPPLID", "BAQPASSWORD"]:
                if var in os.environ:
                    del os.environ[var]
    
    def test_main_with_long_identity(self):
        """Test main with long identity."""
        os.environ["BAQUSERNAME"] = "VERYLONGUSERNAME"
        os.environ["ATSAPPLID"] = "TESTAPP"
        
        try:
            if "BAQPASSWORD" in os.environ:
                del os.environ["BAQPASSWORD"]
            
            result = main()
            
            assert result == 0
            assert "BAQPASSWORD" in os.environ
            assert len(os.environ["BAQPASSWORD"]) == 8
        finally:
            for var in ["BAQUSERNAME", "ATSAPPLID", "BAQPASSWORD"]:
                if var in os.environ:
                    del os.environ[var]
    
    def test_main_racf_failure(self):
        """Test main with RACF failure (empty identity)."""
        # Set empty identity to trigger RACF failure
        os.environ["BAQUSERNAME"] = ""
        os.environ["ATSAPPLID"] = "TESTAPP"
        
        try:
            result = main()
            
            # Should fail because identity is empty
            assert result == 1
        finally:
            for var in ["BAQUSERNAME", "ATSAPPLID"]:
                if var in os.environ:
                    del os.environ[var]


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow(self):
        """Test full flow: get env vars, generate ticket, set password."""
        # Setup
        os.environ["BAQUSERNAME"] = "MYUSER"
        os.environ["ATSAPPLID"] = "MYAPP"
        
        try:
            if "BAQPASSWORD" in os.environ:
                del os.environ["BAQPASSWORD"]
            
            # Run main
            result = main()
            
            # Verify
            assert result == 0
            assert os.environ.get("BAQPASSWORD") is not None
            assert len(os.environ["BAQPASSWORD"]) == 8
            
            # Verify pass ticket format
            passticket = os.environ["BAQPASSWORD"]
            # Should contain chars from identity and applid
            assert len(passticket) == 8
        finally:
            # Cleanup
            for var in ["BAQUSERNAME", "ATSAPPLID", "BAQPASSWORD"]:
                if var in os.environ:
                    del os.environ[var]
    
    def test_multiple_runs(self):
        """Test running main multiple times."""
        os.environ["BAQUSERNAME"] = "TESTUSER"
        os.environ["ATSAPPLID"] = "TESTAPP"
        
        try:
            # First run
            result1 = main()
            assert result1 == 0
            passticket1 = os.environ.get("BAQPASSWORD")
            
            # Second run (should generate new ticket)
            result2 = main()
            assert result2 == 0
            passticket2 = os.environ.get("BAQPASSWORD")
            
            # Both should be valid 8-character tickets
            assert len(passticket1) == 8
            assert len(passticket2) == 8
        finally:
            for var in ["BAQUSERNAME", "ATSAPPLID", "BAQPASSWORD"]:
                if var in os.environ:
                    del os.environ[var]

