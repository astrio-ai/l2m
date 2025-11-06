"""
Test suite for TRADERBL.py

Tests for IMS DL/I trading program.
"""

import pytest
from unittest.mock import MagicMock, patch
from TRADERBL import (
    main,
    IOPCB,
    DBPCBCust,
    DBPCBComp,
    InBuffer,
    OutBuffer,
    Commarea,
    CustomerIOBuffer,
    CompanyIOBuffer,
    DLIDatabase,
    SSACompany,
    SSACustomer,
    SSACustomerIsrt,
    get_company,
    read_custfile,
    read_compfile,
    get_share_value,
    buy_sell,
    buy_sell_buy_function,
    buy_sell_sell_function,
    buy_sell_update_function,
    calculate_shares_bought,
    calculate_shares_sold,
    calculate_share_value,
    build_new_customer,
    build_resp_commarea,
    CLEAN_RETURN,
    UNKNOWN_REQUEST,
    CUSTOMER_NOT_FOUND,
    COMPANY_NOT_FOUND,
    GET_COMPANY_REQ,
    SHARE_VALUE_REQ,
    BUY_SELL_REQ,
    SUBTYPE_BUY,
    SUBTYPE_SELL,
    SUBTYPE_UPDATE,
    INVALID_BUY,
    INVALID_SALE,
    OVERFLOW_RC,
    OVERFLOW_VALUE,
    NORMAL,
)


# ============================================================================
# Test Data Structures
# ============================================================================

class TestCustomerIOBuffer:
    """Test CustomerIOBuffer structure."""
    
    def test_creation(self):
        """Test creating CustomerIOBuffer."""
        cust = CustomerIOBuffer()
        assert cust.customer == ""
        assert cust.company == ""
        assert cust.no_shares == ""
        assert cust.dec_no_shares == 0
    
    def test_sync_shares(self):
        """Test sync_shares method."""
        cust = CustomerIOBuffer()
        cust.dec_no_shares = 100
        cust.sync_shares()
        assert cust.no_shares == "0100"
    
    def test_sync_dec_shares(self):
        """Test sync_dec_shares method."""
        cust = CustomerIOBuffer()
        cust.no_shares = "0100"
        cust.sync_dec_shares()
        assert cust.dec_no_shares == 100


class TestCompanyIOBuffer:
    """Test CompanyIOBuffer structure."""
    
    def test_creation(self):
        """Test creating CompanyIOBuffer."""
        comp = CompanyIOBuffer()
        assert comp.company == ""
        assert comp.share_value_int_part == ""
        assert comp.share_value_dec_part == ""
    
    def test_get_share_value_str(self):
        """Test get_share_value_str method."""
        comp = CompanyIOBuffer()
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        assert comp.get_share_value_str() == "12345.67"
    
    def test_get_share_value_decimal(self):
        """Test get_share_value_decimal method."""
        comp = CompanyIOBuffer()
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        assert float(comp.get_share_value_decimal()) == 12345.67
    
    def test_set_share_value_from_str(self):
        """Test set_share_value_from_str method."""
        comp = CompanyIOBuffer()
        comp.set_share_value_from_str("12345.67")
        assert comp.share_value_int_part == "12345"
        assert comp.share_value_dec_part == "67"


class TestDLIDatabase:
    """Test DLIDatabase mock."""
    
    def test_add_company(self):
        """Test adding a company."""
        db = DLIDatabase()
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        db.add_company(comp)
        assert "TESTCOMP" in db.companies
        assert "TESTCOMP" in db.company_keys
    
    def test_add_customer(self):
        """Test adding a customer."""
        db = DLIDatabase()
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        db.add_customer(cust)
        assert "TESTUSER.TESTCOMP" in db.customers
    
    def test_get_company(self):
        """Test getting a company."""
        db = DLIDatabase()
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        db.add_company(comp)
        result = db.get_company("TESTCOMP")
        assert result is not None
        assert result.company == "TESTCOMP"
    
    def test_get_customer(self):
        """Test getting a customer."""
        db = DLIDatabase()
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        db.add_customer(cust)
        result = db.get_customer("TESTUSER", "TESTCOMP")
        assert result is not None
        assert result.customer == "TESTUSER"
    
    def test_start_company_browse(self):
        """Test starting company browse."""
        db = DLIDatabase()
        for i in range(3):
            comp = CompanyIOBuffer()
            comp.company = f"COMP{i:03d}"
            db.add_company(comp)
        
        db.start_company_browse("")
        company = db.get_next_company()
        assert company is not None
        assert company.company == "COMP000"
    
    def test_get_next_company(self):
        """Test getting next company."""
        db = DLIDatabase()
        for i in range(3):
            comp = CompanyIOBuffer()
            comp.company = f"COMP{i:03d}"
            db.add_company(comp)
        
        db.start_company_browse("")
        company1 = db.get_next_company()
        company2 = db.get_next_company()
        assert company1.company == "COMP000"
        assert company2.company == "COMP001"


# ============================================================================
# Test Business Logic Functions
# ============================================================================

class TestGetCompany:
    """Test get_company function."""
    
    def test_get_company_success(self):
        """Test getting companies successfully."""
        db = DLIDatabase()
        for i in range(3):
            comp = CompanyIOBuffer()
            comp.company = f"COMP{i:03d}"
            db.add_company(comp)
        
        out_buffer = OutBuffer()
        dbpcb_comp = DBPCBComp()
        company_io_buffer = CompanyIOBuffer()
        ssa_company = SSACompany()
        
        get_company(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        
        assert out_buffer.commarea.company_name_tab[0] == "COMP000"
        assert out_buffer.commarea.company_name_tab[1] == "COMP001"
        assert out_buffer.commarea.company_name_tab[2] == "COMP002"
    
    def test_get_company_empty(self):
        """Test getting companies when database is empty."""
        db = DLIDatabase()
        out_buffer = OutBuffer()
        dbpcb_comp = DBPCBComp()
        company_io_buffer = CompanyIOBuffer()
        ssa_company = SSACompany()
        
        get_company(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        
        # Should not crash, but no companies found
        assert dbpcb_comp.db_comp_status != NORMAL


class TestReadCustfile:
    """Test read_custfile function."""
    
    def test_read_custfile_success(self):
        """Test reading customer file successfully."""
        db = DLIDatabase()
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.no_shares = "0100"
        cust.dec_no_shares = 100
        db.add_customer(cust)
        
        out_buffer = OutBuffer()
        out_buffer.commarea.userid = "TESTUSER"
        out_buffer.commarea.company_name = "TESTCOMP"
        dbpcb_cust = DBPCBCust()
        customer_io_buffer = CustomerIOBuffer()
        ssa_customer = SSACustomer()
        
        read_custfile(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert customer_io_buffer.customer == "TESTUSER"
        assert customer_io_buffer.company == "TESTCOMP"
    
    def test_read_custfile_not_found(self):
        """Test reading customer file when not found."""
        db = DLIDatabase()
        out_buffer = OutBuffer()
        out_buffer.commarea.userid = "TESTUSER"
        out_buffer.commarea.company_name = "TESTCOMP"
        dbpcb_cust = DBPCBCust()
        customer_io_buffer = CustomerIOBuffer()
        ssa_customer = SSACustomer()
        
        read_custfile(out_buffer, dbpcb_cust, customer_io_buffer, ssa_customer, db)
        
        assert out_buffer.commarea.return_value == CUSTOMER_NOT_FOUND


class TestReadCompfile:
    """Test read_compfile function."""
    
    def test_read_compfile_success(self):
        """Test reading company file successfully."""
        db = DLIDatabase()
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        out_buffer = OutBuffer()
        out_buffer.commarea.company_name = "TESTCOMP"
        dbpcb_comp = DBPCBComp()
        company_io_buffer = CompanyIOBuffer()
        ssa_company = SSACompany()
        
        read_compfile(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert company_io_buffer.company == "TESTCOMP"
    
    def test_read_compfile_not_found(self):
        """Test reading company file when not found."""
        db = DLIDatabase()
        out_buffer = OutBuffer()
        out_buffer.commarea.company_name = "TESTCOMP"
        dbpcb_comp = DBPCBComp()
        company_io_buffer = CompanyIOBuffer()
        ssa_company = SSACompany()
        
        read_compfile(out_buffer, dbpcb_comp, company_io_buffer, ssa_company, db)
        
        assert out_buffer.commarea.return_value == COMPANY_NOT_FOUND


class TestCalculateShares:
    """Test share calculation functions."""
    
    def test_calculate_shares_bought_success(self):
        """Test calculating shares bought successfully."""
        out_buffer = OutBuffer()
        out_buffer.commarea.no_of_shares_dec = 50
        customer_io_buffer = CustomerIOBuffer()
        customer_io_buffer.dec_no_shares = 100
        
        calculate_shares_bought(out_buffer, customer_io_buffer)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert customer_io_buffer.dec_no_shares == 150
        assert customer_io_buffer.no_shares == "0150"
    
    def test_calculate_shares_bought_overflow(self):
        """Test calculating shares bought with overflow."""
        out_buffer = OutBuffer()
        out_buffer.commarea.no_of_shares_dec = 5000
        customer_io_buffer = CustomerIOBuffer()
        customer_io_buffer.dec_no_shares = 5000
        
        calculate_shares_bought(out_buffer, customer_io_buffer)
        
        assert out_buffer.commarea.return_value == INVALID_BUY
    
    def test_calculate_shares_sold_success(self):
        """Test calculating shares sold successfully."""
        out_buffer = OutBuffer()
        out_buffer.commarea.no_of_shares_dec = 50
        customer_io_buffer = CustomerIOBuffer()
        customer_io_buffer.dec_no_shares = 100
        
        calculate_shares_sold(out_buffer, customer_io_buffer)
        
        assert customer_io_buffer.dec_no_shares == 50
        assert customer_io_buffer.no_shares == "0050"
    
    def test_calculate_share_value_success(self):
        """Test calculating share value successfully."""
        out_buffer = OutBuffer()
        company_io_buffer = CompanyIOBuffer()
        company_io_buffer.share_value_int_part = "12345"
        company_io_buffer.share_value_dec_part = "67"
        customer_io_buffer = CustomerIOBuffer()
        customer_io_buffer.dec_no_shares = 100
        
        calculate_share_value(out_buffer, company_io_buffer, customer_io_buffer)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert "1234567.00" in out_buffer.commarea.total_share_value or "1234567" in out_buffer.commarea.total_share_value


class TestBuySell:
    """Test buy/sell functions."""
    
    def test_buy_sell_buy_success(self):
        """Test buy operation successfully."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        # Add customer
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.dec_no_shares = 100
        cust.no_shares = "0100"
        db.add_customer(cust)
        
        out_buffer = OutBuffer()
        out_buffer.commarea.userid = "TESTUSER"
        out_buffer.commarea.company_name = "TESTCOMP"
        out_buffer.commarea.no_of_shares_dec = 50
        out_buffer.commarea.update_buy_sell = SUBTYPE_BUY
        
        dbpcb_cust = DBPCBCust()
        dbpcb_comp = DBPCBComp()
        customer_io_buffer = CustomerIOBuffer()
        company_io_buffer = CompanyIOBuffer()
        ssa_customer = SSACustomer()
        ssa_customer_isrt = SSACustomerIsrt()
        ssa_company = SSACompany()
        
        buy_sell_buy_function(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_customer_isrt, ssa_company, db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        # Check that customer was updated
        updated_cust = db.get_customer("TESTUSER", "TESTCOMP")
        assert updated_cust.dec_no_shares == 150
    
    def test_buy_sell_sell_success(self):
        """Test sell operation successfully."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        # Add customer
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.dec_no_shares = 100
        cust.no_shares = "0100"
        db.add_customer(cust)
        
        out_buffer = OutBuffer()
        out_buffer.commarea.userid = "TESTUSER"
        out_buffer.commarea.company_name = "TESTCOMP"
        out_buffer.commarea.no_of_shares_dec = 50
        out_buffer.commarea.update_buy_sell = SUBTYPE_SELL
        
        dbpcb_cust = DBPCBCust()
        dbpcb_comp = DBPCBComp()
        customer_io_buffer = CustomerIOBuffer()
        company_io_buffer = CompanyIOBuffer()
        ssa_customer = SSACustomer()
        ssa_company = SSACompany()
        
        buy_sell_sell_function(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_company, db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        # Check that customer was updated
        updated_cust = db.get_customer("TESTUSER", "TESTCOMP")
        assert updated_cust.dec_no_shares == 50
    
    def test_buy_sell_sell_too_many(self):
        """Test sell operation with too many shares."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        db.add_company(comp)
        
        # Add customer
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.dec_no_shares = 100
        cust.no_shares = "0100"
        db.add_customer(cust)
        
        out_buffer = OutBuffer()
        out_buffer.commarea.userid = "TESTUSER"
        out_buffer.commarea.company_name = "TESTCOMP"
        out_buffer.commarea.no_of_shares_dec = 150  # More than owned
        
        dbpcb_cust = DBPCBCust()
        dbpcb_comp = DBPCBComp()
        customer_io_buffer = CustomerIOBuffer()
        company_io_buffer = CompanyIOBuffer()
        ssa_customer = SSACustomer()
        ssa_company = SSACompany()
        
        buy_sell_sell_function(out_buffer, dbpcb_cust, dbpcb_comp, customer_io_buffer, company_io_buffer, ssa_customer, ssa_company, db)
        
        assert out_buffer.commarea.return_value == INVALID_SALE


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_get_company(self, capsys):
        """Test main with GET_COMPANY request."""
        db = DLIDatabase()
        for i in range(3):
            comp = CompanyIOBuffer()
            comp.company = f"COMP{i:03d}"
            db.add_company(comp)
        
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = GET_COMPANY_REQ
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer, db=db)
        
        assert out_buffer.commarea.request_type == GET_COMPANY_REQ
        assert out_buffer.commarea.company_name_tab[0] == "COMP000"
        assert segno == 1
    
    def test_main_get_share_value(self, capsys):
        """Test main with SHARE_VALUE request."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        comp.value_1 = "12340.00"
        comp.value_2 = "12341.00"
        comp.value_3 = "12342.00"
        comp.value_4 = "12343.00"
        comp.value_5 = "12344.00"
        comp.value_6 = "12345.00"
        comp.value_7 = "12346.00"
        comp.commission_buy = "001"
        comp.commission_sell = "002"
        db.add_company(comp)
        
        # Add customer
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.dec_no_shares = 100
        cust.no_shares = "0100"
        db.add_customer(cust)
        
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = SHARE_VALUE_REQ
        in_buffer.commarea.userid = "TESTUSER"
        in_buffer.commarea.company_name = "TESTCOMP"
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer, db=db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert out_buffer.commarea.no_of_shares == "0100"
        assert segno == 1
    
    def test_main_buy_sell(self, capsys):
        """Test main with BUY_SELL request."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        # Add customer
        cust = CustomerIOBuffer()
        cust.customer = "TESTUSER"
        cust.company = "TESTCOMP"
        cust.dec_no_shares = 100
        cust.no_shares = "0100"
        db.add_customer(cust)
        
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = BUY_SELL_REQ
        in_buffer.commarea.userid = "TESTUSER"
        in_buffer.commarea.company_name = "TESTCOMP"
        in_buffer.commarea.no_of_shares_dec = 50
        in_buffer.commarea.update_buy_sell = SUBTYPE_BUY
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer, db=db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert segno == 1
    
    def test_main_unknown_request(self, capsys):
        """Test main with unknown request."""
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = "UNKNOWN_REQUEST"
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer)
        
        assert out_buffer.commarea.return_value == UNKNOWN_REQUEST
        assert segno == 1
    
    def test_main_modname_transform(self, capsys):
        """Test main with MODNAME transformation."""
        iopcb = IOPCB(modname="TRDRQUO", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = GET_COMPANY_REQ
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer)
        
        assert iopcb.modname == "TRDOQUO" or iopcb.modname == ""  # May be cleared after ISRT


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_buy_new_customer(self, capsys):
        """Test full flow: buy shares for new customer."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        # No customer yet - will be created
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = BUY_SELL_REQ
        in_buffer.commarea.userid = "NEWUSER"
        in_buffer.commarea.company_name = "TESTCOMP"
        in_buffer.commarea.no_of_shares_dec = 100
        in_buffer.commarea.update_buy_sell = SUBTYPE_BUY
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer, db=db)
        
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        # Check that customer was created
        new_cust = db.get_customer("NEWUSER", "TESTCOMP")
        assert new_cust is not None
        assert new_cust.dec_no_shares == 100
    
    def test_full_flow_get_share_value_customer_not_found(self, capsys):
        """Test full flow: get share value when customer not found."""
        db = DLIDatabase()
        # Add company
        comp = CompanyIOBuffer()
        comp.company = "TESTCOMP"
        comp.share_value_int_part = "12345"
        comp.share_value_dec_part = "67"
        db.add_company(comp)
        
        # No customer
        iopcb = IOPCB(modname="", userid="TESTUSER")
        in_buffer = InBuffer()
        in_buffer.commarea.request_type = SHARE_VALUE_REQ
        in_buffer.commarea.userid = "UNKNOWNUSER"
        in_buffer.commarea.company_name = "TESTCOMP"
        
        out_buffer, segno = main(iopcb=iopcb, in_buffer=in_buffer, db=db)
        
        # Should return dummy record
        assert out_buffer.commarea.return_value == CLEAN_RETURN
        assert out_buffer.commarea.no_of_shares == "0000"

