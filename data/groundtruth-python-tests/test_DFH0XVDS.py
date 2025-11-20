"""
Test suite for DFH0XVDS.py

Tests for CICS VSAM Data Store operations.
"""

import pytest
from unittest.mock import MagicMock
from DFH0XVDS import (
    main,
    CatalogItem,
    Commarea,
    ErrorMessage,
    VSAMFileHandler,
    CICSOperations,
    catalog_inquire,
    catalog_inquire_single,
    place_order,
    update_file,
    request_not_recognised,
    DFHRESP_NORMAL,
    DFHRESP_NOTFND,
    DFHRESP_ENDFILE,
    REQ_ID_INQUIRE,
    REQ_ID_INQUIRE_SINGLE,
    REQ_ID_PLACE_ORDER,
    RETURN_CODE_SUCCESS,
    RETURN_CODE_NOT_FOUND,
    RETURN_CODE_FILE_ERROR,
    RETURN_CODE_UPDATE_ERROR,
    RETURN_CODE_INSUFFICIENT_STOCK,
    RETURN_CODE_INVALID_QUANTITY,
    RETURN_CODE_UNKNOWN_REQUEST,
    MAX_ITEMS,
)


# ============================================================================
# Test CatalogItem Structure
# ============================================================================

class TestCatalogItem:
    """Test CatalogItem structure."""
    
    def test_creation(self):
        """Test creating CatalogItem."""
        item = CatalogItem()
        assert item.item_ref == "0000"
        assert item.description == ""
        assert item.department == "000"
        assert item.cost == "0.00"
        assert item.in_stock == 0
        assert item.on_order == 0
    
    def test_from_string(self):
        """Test creating from string."""
        # Create properly formatted 80-byte string
        item = CatalogItem(
            item_ref="0001",
            description="Test Item",
            department="001",
            cost="12.34",
            in_stock=10,
            on_order=5
        )
        data = item.to_string()
        parsed_item = CatalogItem.from_string(data)
        assert parsed_item.item_ref == "0001"
        assert parsed_item.description == "Test Item"
        assert parsed_item.department == "001"
        assert parsed_item.cost == "12.34"
        assert parsed_item.in_stock == 10
        assert parsed_item.on_order == 5
    
    def test_to_string(self):
        """Test converting to string."""
        item = CatalogItem(
            item_ref="0001",
            description="Test Item",
            department="001",
            cost="12.34",
            in_stock=10,
            on_order=5
        )
        result = item.to_string()
        assert len(result) == 80
        assert "0001" in result
        assert "Test Item" in result


# ============================================================================
# Test VSAMFileHandler
# ============================================================================

class TestVSAMFileHandler:
    """Test VSAMFileHandler."""
    
    def test_add_record(self):
        """Test adding records."""
        handler = VSAMFileHandler("TESTFILE")
        item = CatalogItem(item_ref="0001", description="Item 1")
        handler.add_record("0001", item)
        assert "0001" in handler._data
        assert handler._data["0001"].description == "Item 1"
    
    def test_startbr(self):
        """Test STARTBR."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        handler.add_record("0002", CatalogItem(item_ref="0002"))
        
        resp_code, key = handler.startbr("0001")
        assert resp_code == DFHRESP_NORMAL
        assert key == "0001"
    
    def test_startbr_not_found(self):
        """Test STARTBR with non-existent key (should find next >= key)."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0002", CatalogItem(item_ref="0002"))
        
        # Should find 0002 (next >= 0001)
        resp_code, key = handler.startbr("0001")
        assert resp_code == DFHRESP_NORMAL
        assert key == "0002"
    
    def test_readnext(self):
        """Test READNEXT."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        handler.add_record("0002", CatalogItem(item_ref="0002"))
        
        handler.startbr("0001")
        # First readnext reads current (0001) and moves to next
        resp_code, item, key = handler.readnext()
        assert resp_code == DFHRESP_NORMAL
        assert item.item_ref == "0001"
        assert key == "0001"
        
        # Second readnext reads 0002
        resp_code, item, key = handler.readnext()
        assert resp_code == DFHRESP_NORMAL
        assert item.item_ref == "0002"
        assert key == "0002"
        
        # Third readnext should be ENDFILE
        resp_code, item, key = handler.readnext()
        assert resp_code == DFHRESP_ENDFILE
    
    def test_read(self):
        """Test READ."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        
        resp_code, item = handler.read("0001")
        assert resp_code == DFHRESP_NORMAL
        assert item.item_ref == "0001"
        
        resp_code, item = handler.read("9999")
        assert resp_code == DFHRESP_NOTFND
        assert item is None
    
    def test_read_update(self):
        """Test READ UPDATE."""
        handler = VSAMFileHandler("TESTFILE")
        item = CatalogItem(item_ref="0001", in_stock=100)
        handler.add_record("0001", item)
        
        resp_code, item = handler.read_update("0001")
        assert resp_code == DFHRESP_NORMAL
        assert item.item_ref == "0001"
        assert handler._locked_key == "0001"
    
    def test_rewrite(self):
        """Test REWRITE."""
        handler = VSAMFileHandler("TESTFILE")
        item = CatalogItem(item_ref="0001", in_stock=100)
        handler.add_record("0001", item)
        
        handler.read_update("0001")
        updated_item = CatalogItem(item_ref="0001", in_stock=90)
        resp_code = handler.rewrite("0001", updated_item)
        assert resp_code == DFHRESP_NORMAL
        assert handler._data["0001"].in_stock == 90
    
    def test_rewrite_not_locked(self):
        """Test REWRITE without lock."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        
        updated_item = CatalogItem(item_ref="0001", in_stock=90)
        resp_code = handler.rewrite("0001", updated_item)
        assert resp_code == DFHRESP_NOTFND
    
    def test_unlock(self):
        """Test UNLOCK."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        handler.read_update("0001")
        
        resp_code = handler.unlock("0001")
        assert resp_code == DFHRESP_NORMAL
        assert handler._locked_key is None
    
    def test_endbr(self):
        """Test ENDBR."""
        handler = VSAMFileHandler("TESTFILE")
        handler.add_record("0001", CatalogItem(item_ref="0001"))
        handler.startbr("0001")
        
        resp_code = handler.endbr()
        assert resp_code == DFHRESP_NORMAL
        assert handler._browse_key is None


# ============================================================================
# Test Catalog Operations
# ============================================================================

class TestCatalogInquire:
    """Test catalog_inquire function."""
    
    def test_success(self):
        """Test successful catalog inquire."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE,
            list_start_ref="0001"
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        # Add test data (starting from 0001)
        for i in range(5):
            item = CatalogItem(
                item_ref=f"{i+1:04d}",
                description=f"Item {i+1}",
                in_stock=10
            )
            vsam_file.add_record(f"{i+1:04d}", item)
        
        catalog_inquire(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_SUCCESS
        # Should read 5 items (0001-0005)
        assert commarea.item_count == 5
        assert commarea.cat_items[0].item_ref == "0001"
        assert "+05 ITEMS RETURNED" in commarea.response_message or "5 ITEMS RETURNED" in commarea.response_message
    
    def test_not_found(self):
        """Test catalog inquire with no records."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE,
            list_start_ref="9999"
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        catalog_inquire(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_NOT_FOUND
        assert commarea.response_message == "ITEM NOT FOUND"
    
    def test_max_items(self):
        """Test catalog inquire with more than MAX_ITEMS."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE,
            list_start_ref="0001"
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        # Add more than MAX_ITEMS
        for i in range(MAX_ITEMS + 5):
            item = CatalogItem(item_ref=f"{i+1:04d}")
            vsam_file.add_record(f"{i+1:04d}", item)
        
        catalog_inquire(commarea, vsam_file, cics_ops)
        
        assert commarea.item_count == MAX_ITEMS
        assert commarea.cat_items[MAX_ITEMS-1].item_ref == f"{MAX_ITEMS:04d}"


class TestCatalogInquireSingle:
    """Test catalog_inquire_single function."""
    
    def test_success(self):
        """Test successful single item inquire."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE_SINGLE,
            item_ref_req="0001"
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        item = CatalogItem(
            item_ref="0001",
            description="Test Item",
            in_stock=10
        )
        vsam_file.add_record("0001", item)
        
        catalog_inquire_single(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_SUCCESS
        assert commarea.single_item.item_ref == "0001"
        assert commarea.single_item.description == "Test Item"
        assert "RETURNED ITEM: REF =0001" in commarea.response_message
    
    def test_not_found(self):
        """Test single item inquire with non-existent item."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE_SINGLE,
            item_ref_req="9999"
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        catalog_inquire_single(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_NOT_FOUND
        assert commarea.response_message == "ITEM NOT FOUND"


class TestPlaceOrder:
    """Test place_order function."""
    
    def test_success(self):
        """Test successful place order."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="0001",
            quantity_req=5
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        item = CatalogItem(
            item_ref="0001",
            description="Test Item",
            in_stock=10
        )
        vsam_file.add_record("0001", item)
        
        place_order(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_SUCCESS
        assert commarea.response_message == "ORDER SUCCESSFULLY PLACED"
        assert vsam_file._data["0001"].in_stock == 5  # 10 - 5
    
    def test_invalid_quantity(self):
        """Test place order with invalid quantity."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="0001",
            quantity_req=0
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        place_order(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_INVALID_QUANTITY
        assert "QUANTITY MUST BE POSITIVE" in commarea.response_message
    
    def test_insufficient_stock(self):
        """Test place order with insufficient stock."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="0001",
            quantity_req=15
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        item = CatalogItem(
            item_ref="0001",
            in_stock=10
        )
        vsam_file.add_record("0001", item)
        
        place_order(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_INSUFFICIENT_STOCK
        assert "INSUFFICENT STOCK" in commarea.response_message
        assert vsam_file._data["0001"].in_stock == 10  # Not updated
    
    def test_item_not_found(self):
        """Test place order with non-existent item."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="9999",
            quantity_req=5
        )
        vsam_file = VSAMFileHandler("TESTFILE")
        cics_ops = CICSOperations()
        
        place_order(commarea, vsam_file, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_NOT_FOUND
        assert "NOT FOUND" in commarea.response_message


class TestRequestNotRecognised:
    """Test request_not_recognised function."""
    
    def test_unknown_request(self):
        """Test unknown request ID."""
        commarea = Commarea(request_id="UNKNOWN")
        cics_ops = CICSOperations()
        
        request_not_recognised(commarea, cics_ops)
        
        assert commarea.return_code == RETURN_CODE_UNKNOWN_REQUEST
        assert commarea.response_message == "OPERATION UNKNOWN"
        assert len(cics_ops.error_queue) == 1
        assert "UNKNOWN REQUEST ID RECEIVED" in cics_ops.error_queue[0]


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_inquire_success(self):
        """Test main with catalog inquire."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE,
            list_start_ref="0001"
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        for i in range(3):
            item = CatalogItem(item_ref=f"{i+1:04d}")
            vsam_file.add_record(f"{i+1:04d}", item)
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_SUCCESS
        assert result.item_count == 3
    
    def test_inquire_single_success(self):
        """Test main with single item inquire."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE_SINGLE,
            item_ref_req="0001"
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        vsam_file.add_record("0001", CatalogItem(item_ref="0001", description="Test"))
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_SUCCESS
        assert result.single_item.item_ref == "0001"
    
    def test_place_order_success(self):
        """Test main with place order."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="0001",
            quantity_req=3
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        vsam_file.add_record("0001", CatalogItem(item_ref="0001", in_stock=10))
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_SUCCESS
        assert result.response_message == "ORDER SUCCESSFULLY PLACED"
        assert vsam_file._data["0001"].in_stock == 7
    
    def test_no_commarea(self):
        """Test main with no commarea."""
        commarea = Commarea()
        cics_ops = CICSOperations()
        
        with pytest.raises(SystemExit) as exc_info:
            main(commarea, eibcalen=0, cics_ops=cics_ops)
        
        assert "ABEND EXCA" in str(exc_info.value)
        assert len(cics_ops.error_queue) == 1
    
    def test_unknown_request(self):
        """Test main with unknown request."""
        commarea = Commarea(request_id="UNKNOWN")
        cics_ops = CICSOperations()
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_UNKNOWN_REQUEST
        assert len(cics_ops.error_queue) == 1
    
    def test_request_id_uppercase(self):
        """Test that request ID is uppercased."""
        commarea = Commarea(
            request_id="01inqc",  # lowercase
            list_start_ref="0001"
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        vsam_file.add_record("0001", CatalogItem(item_ref="0001"))
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        # Should be treated as INQUIRE
        assert result.return_code == RETURN_CODE_SUCCESS
        assert result.item_count == 1


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_inquire_browse(self):
        """Test full flow: inquire with browse."""
        commarea = Commarea(
            request_id=REQ_ID_INQUIRE,
            list_start_ref="0001"
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        for i in range(10):
            item = CatalogItem(
                item_ref=f"{i+1:04d}",
                description=f"Item {i+1}",
                in_stock=10
            )
            vsam_file.add_record(f"{i+1:04d}", item)
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_SUCCESS
        assert result.item_count == 10
        assert all(result.cat_items[i].item_ref == f"{i+1:04d}" for i in range(10))
    
    def test_full_flow_place_order_update_stock(self):
        """Test full flow: place order and update stock."""
        commarea = Commarea(
            request_id=REQ_ID_PLACE_ORDER,
            item_ref_number="0001",
            quantity_req=5
        )
        cics_ops = CICSOperations()
        
        # Use default filename (EXMPCAT )
        vsam_file = cics_ops.get_file("EXMPCAT ")
        original_item = CatalogItem(
            item_ref="0001",
            description="Test Item",
            in_stock=20
        )
        vsam_file.add_record("0001", original_item)
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        assert result.return_code == RETURN_CODE_SUCCESS
        assert vsam_file._data["0001"].in_stock == 15
        assert vsam_file._data["0001"].description == "Test Item"  # Other fields preserved
    
    def test_config_file_read(self):
        """Test configuration file reading."""
        commarea = Commarea(request_id=REQ_ID_INQUIRE)
        cics_ops = CICSOperations()
        
        # Set custom filename
        cics_ops.config_file.set_config("VSAM-NAME", "CUSTOM  ")
        
        vsam_file = cics_ops.get_file("CUSTOM  ")
        vsam_file.add_record("0001", CatalogItem(item_ref="0001"))
        
        result = main(commarea, eibcalen=100, cics_ops=cics_ops)
        
        # Should use custom filename
        assert "CUSTOM  " in cics_ops.files

