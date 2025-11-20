"""
Test suite for CORPT00C.py

Tests for Transaction Report transaction program.
"""

import pytest
from unittest.mock import patch
from datetime import datetime

from CORPT00C import (
    ReportMap,
    DateValidationResult,
    WorkingStorage,
    CardDemoCommarea,
    TDQHandler,
    validate_date,
    build_jcl_template,
    populate_header_info,
    send_trnrpt_screen,
    receive_trnrpt_screen,
    initialize_all_fields,
    process_monthly_report,
    process_yearly_report,
    process_custom_report,
    process_enter_key,
    submit_job_to_intrdr,
    write_jobsub_tdq,
    return_to_prev_screen,
    main,
    WS_PGMNAME,
    WS_TRANID,
    DFHENTER,
    DFHPF3,
    COSGN00C,
    COMEN01C,
    TDQ_QUEUE_NAME,
    DFHRESP_NORMAL
)


# ============================================================================
# Test Date Validation
# ============================================================================

class TestDateValidation:
    """Test date validation function."""
    
    def test_validate_date_valid(self):
        """Test validating a valid date."""
        result = validate_date("2025-12-31")
        assert result.sev_cd == "0000"
        assert "valid" in result.msg.lower()
    
    def test_validate_date_invalid(self):
        """Test validating an invalid date."""
        result = validate_date("2025-13-01")  # Invalid month
        assert result.sev_cd != "0000"
    
    def test_validate_date_invalid_day(self):
        """Test validating a date with invalid day."""
        result = validate_date("2025-02-30")  # Invalid day for February
        assert result.sev_cd != "0000"
    
    def test_validate_date_invalid_format(self):
        """Test validating a date with invalid format."""
        result = validate_date("2025/12/31")  # Wrong separator
        assert result.sev_cd != "0000"


# ============================================================================
# Test JCL Generation
# ============================================================================

class TestJCLGeneration:
    """Test JCL generation."""
    
    def test_build_jcl_template(self):
        """Test building JCL template."""
        start_date = "2025-01-01"
        end_date = "2025-12-31"
        
        jcl_lines = build_jcl_template(start_date, end_date)
        
        assert len(jcl_lines) > 0
        assert "TRNRPT00" in jcl_lines[0]
        assert start_date in jcl_lines[10]  # PARM-START-DATE line
        assert end_date in jcl_lines[11]  # PARM-END-DATE line
        assert "/*EOF" in jcl_lines[-1]


# ============================================================================
# Test TDQ Handler
# ============================================================================

class TestTDQHandler:
    """Test TDQ handler."""
    
    def test_writeq_td_success(self):
        """Test successful write to TDQ."""
        handler = TDQHandler(TDQ_QUEUE_NAME)
        resp_cd = handler.writeq_td("TEST RECORD")
        assert resp_cd == 0
        assert len(handler.get_queue_contents()) == 1
    
    def test_writeq_td_multiple_records(self):
        """Test writing multiple records to TDQ."""
        handler = TDQHandler(TDQ_QUEUE_NAME)
        handler.writeq_td("RECORD 1")
        handler.writeq_td("RECORD 2")
        handler.writeq_td("RECORD 3")
        
        contents = handler.get_queue_contents()
        assert len(contents) == 3
        assert "RECORD 1" in contents
        assert "RECORD 2" in contents
        assert "RECORD 3" in contents


# ============================================================================
# Test Map Functions
# ============================================================================

class TestMapFunctions:
    """Test map functions."""
    
    def test_populate_header_info(self):
        """Test populating header info."""
        ws = WorkingStorage()
        map_out = ReportMap()
        
        populate_header_info(ws, map_out)
        
        assert map_out.title01o != ""
        assert map_out.trnnameo == WS_TRANID
        assert map_out.pgmnameo == WS_PGMNAME
        assert map_out.curdateo != ""
        assert map_out.curtimeo != ""
    
    def test_initialize_all_fields(self):
        """Test initializing all fields."""
        ws = WorkingStorage()
        map_out = ReportMap()
        map_out.monthlyi = "X"
        map_out.sdtmmi = "01"
        ws.message = "Test"
        
        initialize_all_fields(ws, map_out)
        
        assert map_out.monthlyi == ""
        assert map_out.sdtmmi == ""
        assert ws.message == ""
        assert map_out.monthlyl == -1


# ============================================================================
# Test Report Processing
# ============================================================================

class TestReportProcessing:
    """Test report processing functions."""
    
    def test_process_monthly_report(self):
        """Test processing monthly report."""
        ws = WorkingStorage()
        
        start_date, end_date = process_monthly_report(ws)
        
        assert ws.report_name == "Monthly"
        assert start_date != ""
        assert end_date != ""
        assert "-" in start_date
        assert "-" in end_date
        assert start_date[:4] == end_date[:4]  # Same year
        assert start_date[5:7] == end_date[5:7]  # Same month
    
    def test_process_yearly_report(self):
        """Test processing yearly report."""
        ws = WorkingStorage()
        
        start_date, end_date = process_yearly_report(ws)
        
        assert ws.report_name == "Yearly"
        assert start_date != ""
        assert end_date != ""
        assert start_date.endswith("-01-01")  # January 1
        assert end_date.endswith("-12-31")  # December 31
        assert start_date[:4] == end_date[:4]  # Same year
    
    def test_process_custom_report_valid(self):
        """Test processing valid custom report."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.sdtmmi = "01"
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_out = ReportMap()
        
        result = process_custom_report(ws, map_in, map_out)
        
        assert result is not None
        assert ws.report_name == "Custom"
        assert result[0] == "2025-01-01"
        assert result[1] == "2025-12-31"
        assert ws.err_flg is False
    
    def test_process_custom_report_missing_month(self):
        """Test processing custom report with missing month."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.sdtmmi = ""
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_out = ReportMap()
        
        result = process_custom_report(ws, map_in, map_out)
        
        assert result is None
        assert ws.err_flg is True
        assert "Month" in ws.message
        assert map_out.sdtmml == -1
    
    def test_process_custom_report_invalid_month(self):
        """Test processing custom report with invalid month."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.sdtmmi = "13"
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_out = ReportMap()
        
        result = process_custom_report(ws, map_in, map_out)
        
        assert result is None
        assert ws.err_flg is True
        assert "valid Month" in ws.message
    
    def test_process_custom_report_invalid_date(self):
        """Test processing custom report with invalid date."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.sdtmmi = "02"
        map_in.sdtddi = "30"  # Invalid day for February
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_out = ReportMap()
        
        result = process_custom_report(ws, map_in, map_out)
        
        assert result is None
        assert ws.err_flg is True
        assert "valid date" in ws.message


# ============================================================================
# Test Process Enter Key
# ============================================================================

class TestProcessEnterKey:
    """Test process_enter_key function."""
    
    def test_process_enter_key_monthly(self):
        """Test processing enter key with monthly report."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.monthlyi = "X"
        map_out = ReportMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is not None
        assert ws.report_name == "Monthly"
    
    def test_process_enter_key_yearly(self):
        """Test processing enter key with yearly report."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.yearlyi = "X"
        map_out = ReportMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is not None
        assert ws.report_name == "Yearly"
    
    def test_process_enter_key_custom(self):
        """Test processing enter key with custom report."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_in.customi = "X"
        map_in.sdtmmi = "01"
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_out = ReportMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is not None
        assert ws.report_name == "Custom"
    
    def test_process_enter_key_no_selection(self):
        """Test processing enter key with no report type selected."""
        ws = WorkingStorage()
        map_in = ReportMap()
        map_out = ReportMap()
        
        result = process_enter_key(ws, map_in, map_out)
        
        assert result is None
        assert ws.err_flg is True
        assert "Select a report type" in ws.message


# ============================================================================
# Test Submit Job
# ============================================================================

class TestSubmitJob:
    """Test submit job functions."""
    
    def test_write_jobsub_tdq_success(self):
        """Test successful write to TDQ."""
        ws = WorkingStorage()
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        
        success = write_jobsub_tdq(ws, "TEST JCL RECORD")
        
        assert success is True
        assert ws.err_flg is False
        assert len(ws.tdq_handler.get_queue_contents()) == 1
    
    def test_submit_job_no_confirmation(self):
        """Test submitting job without confirmation."""
        ws = WorkingStorage()
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        map_in = ReportMap()
        map_in.confirmy = ""
        map_out = ReportMap()
        
        success = submit_job_to_intrdr(ws, map_in, map_out, "2025-01-01", "2025-12-31")
        
        assert success is False
        assert ws.err_flg is True
        assert "confirm" in ws.message.lower()
    
    def test_submit_job_cancelled(self):
        """Test submitting job with N confirmation."""
        ws = WorkingStorage()
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        map_in = ReportMap()
        map_in.confirmy = "N"
        map_out = ReportMap()
        
        success = submit_job_to_intrdr(ws, map_in, map_out, "2025-01-01", "2025-12-31")
        
        assert success is False
        assert ws.err_flg is True
    
    def test_submit_job_success(self):
        """Test successful job submission."""
        ws = WorkingStorage()
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        map_in = ReportMap()
        map_in.confirmy = "Y"
        map_out = ReportMap()
        ws.report_name = "Monthly"
        
        success = submit_job_to_intrdr(ws, map_in, map_out, "2025-01-01", "2025-12-31")
        
        assert success is True
        assert ws.err_flg is False
        assert len(ws.tdq_handler.get_queue_contents()) > 0
        assert "TRNRPT00" in ws.tdq_handler.get_queue_contents()[0]
    
    def test_submit_job_invalid_confirmation(self):
        """Test submitting job with invalid confirmation."""
        ws = WorkingStorage()
        ws.tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        map_in = ReportMap()
        map_in.confirmy = "X"
        map_out = ReportMap()
        
        success = submit_job_to_intrdr(ws, map_in, map_out, "2025-01-01", "2025-12-31")
        
        assert success is False
        assert ws.err_flg is True
        assert "not a valid value" in ws.message


# ============================================================================
# Test Main Function
# ============================================================================

class TestMainFunction:
    """Test main function."""
    
    def test_main_first_entry(self):
        """Test main on first entry."""
        commarea = CardDemoCommarea()
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=0
        )
        
        assert xctl_prog == COSGN00C
    
    def test_main_first_screen_display(self):
        """Test main showing screen for first time."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        assert result_commarea.pgm_reenter is True
        assert result_map.title01o != ""
        assert result_map.monthlyl == -1
        assert xctl_prog is None
    
    def test_main_pf3_exit(self):
        """Test main with PF3."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog == COMEN01C
    
    def test_main_monthly_report_success(self):
        """Test main with monthly report submission."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = ReportMap()
        map_in.monthlyi = "X"
        map_in.confirmy = "Y"
        tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tdq_queue=tdq_handler
        )
        
        assert xctl_prog is None
        assert "submitted" in result_map.errmsgo.lower()
        assert result_map.errmsgc == "DFHGREEN"
        assert len(tdq_handler.get_queue_contents()) > 0
    
    def test_main_custom_report_valid(self):
        """Test main with valid custom report."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = ReportMap()
        map_in.customi = "X"
        map_in.sdtmmi = "01"
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        map_in.confirmy = "Y"
        tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tdq_queue=tdq_handler
        )
        
        assert xctl_prog is None
        assert "submitted" in result_map.errmsgo.lower()
        assert len(tdq_handler.get_queue_contents()) > 0
    
    def test_main_custom_report_invalid(self):
        """Test main with invalid custom report."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = ReportMap()
        map_in.customi = "X"
        map_in.sdtmmi = "13"  # Invalid month
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "12"
        map_in.edtddi = "31"
        map_in.edtyyyyi = "2025"
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "valid Month" in result_map.errmsgo
    
    def test_main_no_confirmation(self):
        """Test main with no confirmation."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = ReportMap()
        map_in.monthlyi = "X"
        map_in.confirmy = ""  # No confirmation
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in
        )
        
        assert xctl_prog is None
        assert "confirm" in result_map.errmsgo.lower()


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests."""
    
    def test_full_flow_monthly_report(self):
        """Test full flow: monthly report submission."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            tdq_queue=tdq_handler
        )
        
        assert result_map.title01o != ""
        
        # Submit monthly report
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        map_in = ReportMap()
        map_in.monthlyi = "X"
        map_in.confirmy = "Y"
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tdq_queue=tdq_handler
        )
        
        assert "submitted" in result_map2.errmsgo.lower()
        assert len(tdq_handler.get_queue_contents()) > 0
        # Verify JCL contains date parameters
        jcl_contents = " ".join(tdq_handler.get_queue_contents())
        assert "PARM-START-DATE" in jcl_contents
        assert "PARM-END-DATE" in jcl_contents
    
    def test_full_flow_custom_report(self):
        """Test full flow: custom report submission."""
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = True
        map_in = ReportMap()
        map_in.customi = "X"
        map_in.sdtmmi = "06"
        map_in.sdtddi = "01"
        map_in.sdtyyyyi = "2025"
        map_in.edtmmi = "06"
        map_in.edtddi = "30"
        map_in.edtyyyyi = "2025"
        map_in.confirmy = "Y"
        tdq_handler = TDQHandler(TDQ_QUEUE_NAME)
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100,
            eibaid=DFHENTER,
            map_input=map_in,
            tdq_queue=tdq_handler
        )
        
        assert "submitted" in result_map.errmsgo.lower()
        assert len(tdq_handler.get_queue_contents()) > 0
        # Verify JCL contains custom dates
        jcl_contents = " ".join(tdq_handler.get_queue_contents())
        assert "2025-06-01" in jcl_contents
        assert "2025-06-30" in jcl_contents
    
    def test_full_flow_pf3_exit(self):
        """Test full flow: screen display and PF3 exit."""
        # First entry - show screen
        commarea = CardDemoCommarea()
        commarea.pgm_reenter = False
        
        result_commarea, result_map, xctl_prog = main(
            commarea=commarea,
            eibcalen=100
        )
        
        # PF3 to exit
        commarea2 = CardDemoCommarea()
        commarea2.pgm_reenter = True
        
        result_commarea2, result_map2, xctl_prog2 = main(
            commarea=commarea2,
            eibcalen=100,
            eibaid=DFHPF3
        )
        
        assert xctl_prog2 == COMEN01C

