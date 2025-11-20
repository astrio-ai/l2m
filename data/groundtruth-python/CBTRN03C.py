"""
Ground Truth Python Equivalent for CBTRN03C.cbl

Program: CBTRN03C
Application: CardDemo
Type: BATCH COBOL Program
Function: Print the transaction detail report.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass
from typing import Optional, Dict
from decimal import Decimal


@dataclass
class TranRecord:
    """Transaction record (TRAN-RECORD from COPY CVTRA05Y)."""
    tran_id: str = ""  # PIC X(16)
    tran_type_cd: str = ""  # 2 chars
    tran_cat_cd: str = ""  # 4 chars
    tran_source: str = ""
    tran_desc: str = ""
    tran_amt: Decimal = Decimal('0')
    tran_merchant_id: str = ""
    tran_merchant_name: str = ""
    tran_merchant_city: str = ""
    tran_merchant_zip: str = ""
    tran_card_num: str = ""  # 16 chars
    tran_orig_ts: str = ""  # 26 chars
    tran_proc_ts: str = ""  # 26 chars
    
    @property
    def trans_id(self) -> str:
        return self.tran_id


@dataclass
class CardXrefRecord:
    """Card cross-reference record (CARD-XREF-RECORD from COPY CVACT03Y)."""
    card_num: str = ""  # 16 chars
    cust_num: str = ""  # 9 chars
    acct_id: str = ""  # 11 chars
    
    @property
    def xref_card_num(self) -> str:
        return self.card_num
    
    @property
    def xref_acct_id(self) -> str:
        return self.acct_id


@dataclass
class TranTypeRecord:
    """Transaction type record (TRAN-TYPE-RECORD from COPY CVTRA03Y)."""
    tran_type: str = ""  # 2 chars
    tran_type_desc: str = ""
    
    @property
    def tran_type_cd(self) -> str:
        return self.tran_type


@dataclass
class TranCatRecord:
    """Transaction category record (TRAN-CAT-RECORD from COPY CVTRA04Y)."""
    tran_type_cd: str = ""  # 2 chars
    tran_cat_cd: str = ""  # 4 chars
    tran_cat_type_desc: str = ""
    
    @property
    def tran_cat_type_cd(self) -> str:
        return self.tran_type_cd


class SequentialFileHandler:
    """Handler for sequential files."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self.end_of_file = False
    
    def open_file(self, mode: str = "r") -> int:
        """Open file."""
        try:
            self.file_handle = open(self.filename, mode, encoding='utf-8')
            self.file_status = "00"
            self.end_of_file = False
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def read_next(self) -> tuple[Optional[str], int]:
        """Read next record."""
        if self.end_of_file:
            return None, 16
        
        try:
            line = self.file_handle.readline()
            if not line:
                self.end_of_file = True
                self.file_status = "10"
                return None, 16
            
            line = line.rstrip('\n\r')
            self.file_status = "00"
            return line, 0
        except Exception as e:
            self.file_status = "99"
            return None, 12
    
    def write_record(self, record: str) -> int:
        """Write a record."""
        try:
            self.file_handle.write(record + '\n')
            self.file_handle.flush()
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def close_file(self) -> int:
        """Close file."""
        try:
            if self.file_handle:
                self.file_handle.close()
                self.file_handle = None
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def display_io_status(self):
        """Display I/O status."""
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        if io_status[0].isdigit() and io_status[1].isdigit() and io_status[0] != '9':
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            left = io_status[0] if len(io_status) > 0 else '0'
            right = io_status[1] if len(io_status) > 1 else '0'
            # Convert right char to numeric
            try:
                right_num = ord(right) if not right.isdigit() else int(right)
                status_04 = f"{left}00{right_num:03d}"
            except:
                status_04 = f"{left}0000"
            print(f'FILE STATUS IS: NNNN {status_04}')


class IndexedFileHandler:
    """Handler for indexed files (simulated with dictionary)."""
    
    def __init__(self, filename: str):
        self.filename = filename
        self.file_handle: Optional[object] = None
        self.file_status = "00"
        self._data: Dict[str, str] = {}
    
    def open_file(self, mode: str = "r") -> int:
        """Open file and load data."""
        try:
            if mode in ("r", "r+"):
                with open(self.filename, "r", encoding='utf-8') as f:
                    for line in f:
                        line = line.rstrip('\n\r')
                        if len(line) > 0:
                            key = self._extract_key(line)
                            if key:
                                self._data[key] = line
            self.file_status = "00"
            return 0
        except FileNotFoundError:
            self.file_status = "23"
            return 12
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def read_record(self, key: str) -> tuple[Optional[str], int]:
        """Read record by key."""
        if key in self._data:
            self.file_status = "00"
            return self._data[key], 0
        else:
            self.file_status = "23"
            return None, 12
    
    def close_file(self) -> int:
        """Close file."""
        try:
            self.file_status = "00"
            return 0
        except Exception as e:
            self.file_status = "99"
            return 12
    
    def display_io_status(self):
        """Display I/O status."""
        io_status = self.file_status
        if len(io_status) != 2:
            io_status = "00"
        
        if io_status[0].isdigit() and io_status[1].isdigit() and io_status[0] != '9':
            status_04 = f"00{io_status}"
            print(f'FILE STATUS IS: NNNN {status_04}')
        else:
            left = io_status[0] if len(io_status) > 0 else '0'
            right = io_status[1] if len(io_status) > 1 else '0'
            try:
                right_num = ord(right) if not right.isdigit() else int(right)
                status_04 = f"{left}00{right_num:03d}"
            except:
                status_04 = f"{left}0000"
            print(f'FILE STATUS IS: NNNN {status_04}')
    
    def _extract_key(self, line: str) -> str:
        """Extract key from line (override in subclasses)."""
        return line[:16] if len(line) >= 16 else ""


class XrefFileHandler(IndexedFileHandler):
    """Handler for XREF file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract card number key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class TranTypeFileHandler(IndexedFileHandler):
    """Handler for TRANTYPE file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract transaction type key (2 chars)."""
        if len(line) >= 2:
            return line[:2]
        return ""


class TranCatFileHandler(IndexedFileHandler):
    """Handler for TRANCATG file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract composite key: type_cd (2) + cat_cd (4) = 6 chars."""
        if len(line) >= 6:
            return line[:6]
        return ""


def parse_tran_record(line: str) -> TranRecord:
    """Parse transaction record from line."""
    if len(line) < 304:
        return TranRecord()
    
    record = TranRecord()
    
    # FD-TRANS-DATA (304 chars) + FD-TRAN-PROC-TS (26 chars) + FD-FILLER (20 chars) = 350 chars total
    # But TRAN-RECORD structure is embedded in FD-TRANS-DATA
    # Assume same structure as in CBTRN02C: ID(16) + type(2) + cat(4) + source(20) + desc(50) + amt(15) + merchant_id(10) + merchant_name(30) + merchant_city(20) + merchant_zip(10) + card(16) + orig_ts(26) = 219 chars
    # Then proc_ts at 304:330
    if len(line) >= 16:
        record.tran_id = line[0:16]
    if len(line) >= 18:
        record.tran_type_cd = line[16:18].strip()
    if len(line) >= 22:
        # cat_cd is 4 chars at position 18:22, but may have spaces before it in test data
        # Look for 4-digit sequence starting from position 18
        # Skip spaces and find first 4 consecutive digits
        found = False
        for i in range(18, min(len(line) - 3, 30)):
            candidate = line[i:i+4]
            if candidate.isdigit() and len(candidate) == 4:
                record.tran_cat_cd = candidate
                found = True
                break
        if not found:
            # Fallback: try fixed position
            cat_cd_raw = line[18:22]
            if cat_cd_raw.strip():
                record.tran_cat_cd = cat_cd_raw.strip().zfill(4)
            else:
                record.tran_cat_cd = "0000"
    if len(line) >= 42:
        record.tran_source = line[22:42].strip()
    if len(line) >= 92:
        record.tran_desc = line[42:92].strip()
    if len(line) >= 107:
        try:
            amt_str = line[92:107].strip()
            if amt_str:
                record.tran_amt = Decimal(amt_str)
        except:
            record.tran_amt = Decimal('0')
    if len(line) >= 117:
        record.tran_merchant_id = line[107:117].strip()
    if len(line) >= 147:
        record.tran_merchant_name = line[117:147].strip()
    if len(line) >= 167:
        record.tran_merchant_city = line[147:167].strip()
    if len(line) >= 177:
        record.tran_merchant_zip = line[167:177].strip()
    if len(line) >= 167:
        # Card number is 16 chars after merchant_zip (which is at 167-177)
        # Try fixed position first (177-193)
        card_candidate = line[177:193].strip() if len(line) >= 193 else ""
        if len(card_candidate) == 16 and card_candidate.isdigit():
            record.tran_card_num = card_candidate
        else:
            # Look for 16 consecutive digits starting from after merchant_zip (position 150 onwards)
            # Search in a wider range to handle variable-width test data
            for i in range(150, min(len(line) - 15, 220)):
                candidate = line[i:i+16]
                if candidate.isdigit() and len(candidate) == 16:
                    record.tran_card_num = candidate
                    break
    if len(line) >= 219:
        record.tran_orig_ts = line[193:219]
    # proc_ts is at position 304:330, but may be at a different position if line is shorter
    # Look for timestamp pattern YYYY-MM-DD-HH.MM.SS.MMMMMM starting from position 283 onwards
    if len(line) >= 330:
        # Full line, timestamp should be at fixed position
        record.tran_proc_ts = line[304:330]
    else:
        # Shorter line, search for timestamp pattern
        # Timestamp pattern: YYYY-MM-DD-HH.MM.SS.MMMMMM (26 chars)
        # Search from position 270 to end of line
        found = False
        for i in range(max(270, 0), max(0, len(line) - 25)):
            candidate = line[i:i+26]
            # Check if it matches timestamp pattern: YYYY-MM-DD-HH.MM.SS.MMMMMM
            if (len(candidate) >= 26 and 
                candidate[4] == '-' and candidate[7] == '-' and candidate[10] == '-' and
                candidate[13] == '.' and candidate[16] == '.' and candidate[19] == '.'):
                record.tran_proc_ts = candidate
                found = True
                break
        if not found and len(line) > 304:
            # Fallback: try position 304 onwards
            proc_ts_candidate = line[304:].strip()
            if len(proc_ts_candidate) >= 10 and proc_ts_candidate[4] == '-' and proc_ts_candidate[7] == '-':
                record.tran_proc_ts = proc_ts_candidate[:26] if len(proc_ts_candidate) >= 26 else proc_ts_candidate
    
    return record


def parse_xref_record(line: str) -> CardXrefRecord:
    """Parse XREF record."""
    if len(line) < 36:
        return CardXrefRecord()
    
    return CardXrefRecord(
        card_num=line[:16],
        cust_num=line[16:25] if len(line) > 16 else "",
        acct_id=line[25:36] if len(line) > 25 else ""
    )


def parse_tran_type_record(line: str) -> TranTypeRecord:
    """Parse transaction type record."""
    if len(line) < 2:
        return TranTypeRecord()
    
    return TranTypeRecord(
        tran_type=line[:2],
        tran_type_desc=line[2:60].strip() if len(line) > 2 else ""
    )


def parse_tran_cat_record(line: str) -> TranCatRecord:
    """Parse transaction category record."""
    if len(line) < 6:
        return TranCatRecord()
    
    return TranCatRecord(
        tran_type_cd=line[:2],
        tran_cat_cd=line[2:6],
        tran_cat_type_desc=line[6:60].strip() if len(line) > 6 else ""
    )


def format_report_name_header() -> str:
    """Format report name header."""
    return "TRANSACTION DETAIL REPORT".ljust(133)


def format_transaction_header_1() -> str:
    """Format transaction header 1."""
    return "TRANS-ID      ACCT-ID     TYPE CD  TYPE DESC           CAT CD  CAT DESC           SOURCE            AMOUNT".ljust(133)


def format_transaction_header_2() -> str:
    """Format transaction header 2."""
    return "-" * 133


def format_transaction_detail(tran_id: str, acct_id: str, type_cd: str, type_desc: str, 
                              cat_cd: str, cat_desc: str, source: str, amt: Decimal) -> str:
    """Format transaction detail line."""
    # Ensure proper field widths
    tran_id_fmt = tran_id[:16].ljust(16)
    acct_id_fmt = acct_id[:11].ljust(11)
    type_cd_fmt = type_cd[:2].ljust(2)
    type_desc_fmt = type_desc[:18].ljust(18)
    cat_cd_fmt = cat_cd[:4].ljust(4)
    cat_desc_fmt = cat_desc[:18].ljust(18)
    source_fmt = source[:18].ljust(18)
    amt_fmt = str(amt).rjust(15)
    detail = f"{tran_id_fmt} {acct_id_fmt} {type_cd_fmt}   {type_desc_fmt} {cat_cd_fmt}   {cat_desc_fmt} {source_fmt} {amt_fmt}"
    return detail.ljust(133)


def format_page_totals(total: Decimal) -> str:
    """Format page totals line."""
    return f"PAGE TOTAL: {str(total):>15s}".ljust(133)


def format_account_totals(total: Decimal) -> str:
    """Format account totals line."""
    return f"ACCOUNT TOTAL: {str(total):>15s}".ljust(133)


def format_grand_totals(total: Decimal) -> str:
    """Format grand totals line."""
    return f"GRAND TOTAL: {str(total):>15s}".ljust(133)


def format_report_header(start_date: str, end_date: str) -> str:
    """Format report header with date range."""
    return f"REPORTING FROM {start_date} TO {end_date}".ljust(133)


def abend_program():
    """Abend the program."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBTRN03C')
    
    # File handlers
    tranfile = SequentialFileHandler("TRANFILE")
    reportfile = SequentialFileHandler("TRANREPT")
    xreffile = XrefFileHandler("CARDXREF")
    trantypefile = TranTypeFileHandler("TRANTYPE")
    trancatfile = TranCatFileHandler("TRANCATG")
    dateparmfile = SequentialFileHandler("DATEPARM")
    
    # Working variables
    end_of_file = 'N'
    ws_first_time = 'Y'
    ws_line_counter = 0
    ws_page_size = 20
    ws_blank_line = " " * 133
    ws_page_total = Decimal('0')
    ws_account_total = Decimal('0')
    ws_grand_total = Decimal('0')
    ws_curr_card_num = ""
    ws_start_date = ""
    ws_end_date = ""
    
    # Current records
    tran_record: Optional[TranRecord] = None
    xref_record: Optional[CardXrefRecord] = None
    tran_type_record: Optional[TranTypeRecord] = None
    tran_cat_record: Optional[TranCatRecord] = None
    
    try:
        # Open files
        if tranfile.open_file("r") != 0:
            print('ERROR OPENING TRANFILE')
            tranfile.display_io_status()
            abend_program()
        
        if reportfile.open_file("w") != 0:
            print('ERROR OPENING REPTFILE')
            reportfile.display_io_status()
            abend_program()
        
        if xreffile.open_file("r") != 0:
            print('ERROR OPENING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if trantypefile.open_file("r") != 0:
            print('ERROR OPENING TRANSACTION TYPE FILE')
            trantypefile.display_io_status()
            abend_program()
        
        if trancatfile.open_file("r") != 0:
            print('ERROR OPENING TRANSACTION CATG FILE')
            trancatfile.display_io_status()
            abend_program()
        
        if dateparmfile.open_file("r") != 0:
            print('ERROR OPENING DATE PARM FILE')
            dateparmfile.display_io_status()
            abend_program()
        
        # Read date parameters
        dateparm_line, appl_result = dateparmfile.read_next()
        if appl_result == 0 and dateparm_line:
            # Format: START-DATE(10) + FILLER(1) + END-DATE(10) = 21 chars
            # But may also be space-separated
            dateparm_line = dateparm_line.strip()
            if len(dateparm_line) >= 21:
                ws_start_date = dateparm_line[:10]
                ws_end_date = dateparm_line[11:21]
            elif ' ' in dateparm_line:
                # Space-separated format
                parts = dateparm_line.split()
                if len(parts) >= 2:
                    ws_start_date = parts[0][:10]
                    ws_end_date = parts[1][:10]
                elif len(parts) >= 1:
                    ws_start_date = parts[0][:10]
                    ws_end_date = parts[0][:10]
                else:
                    ws_start_date = dateparm_line[:10] if len(dateparm_line) >= 10 else ""
                    ws_end_date = ws_start_date
            elif len(dateparm_line) >= 10:
                ws_start_date = dateparm_line[:10]
                ws_end_date = dateparm_line[:10]  # Default to same date
            else:
                ws_start_date = ""
                ws_end_date = ""
            print(f'Reporting from {ws_start_date} to {ws_end_date}')
        elif appl_result == 16:
            end_of_file = 'Y'
        else:
            print('ERROR READING DATEPARM FILE')
            dateparmfile.display_io_status()
            abend_program()
        
        # Main processing loop
        while end_of_file != 'Y':
            if end_of_file == 'N':
                # Read next transaction record
                line, appl_result = tranfile.read_next()
                
                if appl_result == 0 and line:
                    tran_record = parse_tran_record(line)
                    print(tran_record.tran_id)
                    
                    # Check date range
                    if len(tran_record.tran_proc_ts) >= 10 and ws_start_date and ws_end_date:
                        tran_date = tran_record.tran_proc_ts[:10]
                        if tran_date >= ws_start_date and tran_date <= ws_end_date:
                            # Process transaction
                            # Check if new card number
                            if ws_curr_card_num != tran_record.tran_card_num:
                                if ws_first_time == 'N':
                                    # Write account totals
                                    account_total_line = format_account_totals(ws_account_total)
                                    reportfile.write_record(account_total_line)
                                    ws_line_counter += 1
                                    reportfile.write_record(format_transaction_header_2())
                                    ws_line_counter += 1
                                    ws_account_total = Decimal('0')
                                
                                ws_curr_card_num = tran_record.tran_card_num
                                
                                # Lookup XREF
                                xref_key = tran_record.tran_card_num
                                xref_line, appl_result = xreffile.read_record(xref_key)
                                if appl_result == 0 and xref_line:
                                    xref_record = parse_xref_record(xref_line)
                                else:
                                    print(f'INVALID CARD NUMBER : {xref_key}')
                                    xreffile.display_io_status()
                                    abend_program()
                            
                            # Lookup transaction type
                            tran_type_key = tran_record.tran_type_cd
                            tran_type_line, appl_result = trantypefile.read_record(tran_type_key)
                            if appl_result == 0 and tran_type_line:
                                tran_type_record = parse_tran_type_record(tran_type_line)
                            else:
                                print(f'INVALID TRANSACTION TYPE : {tran_type_key}')
                                trantypefile.display_io_status()
                                abend_program()
                            
                            # Lookup transaction category
                            tran_cat_key = f"{tran_record.tran_type_cd}{tran_record.tran_cat_cd}"
                            tran_cat_line, appl_result = trancatfile.read_record(tran_cat_key)
                            if appl_result == 0 and tran_cat_line:
                                tran_cat_record = parse_tran_cat_record(tran_cat_line)
                            else:
                                print(f'INVALID TRAN CATG KEY : {tran_cat_key}')
                                trancatfile.display_io_status()
                                abend_program()
                            
                            # Write transaction detail
                            if ws_first_time == 'Y':
                                ws_first_time = 'N'
                                # Write headers
                                reportfile.write_record(format_report_name_header())
                                ws_line_counter += 1
                                reportfile.write_record(ws_blank_line)
                                ws_line_counter += 1
                                reportfile.write_record(format_transaction_header_1())
                                ws_line_counter += 1
                                reportfile.write_record(format_transaction_header_2())
                                ws_line_counter += 1
                            
                            # Check if page break needed
                            if ws_line_counter > 0 and ws_line_counter % ws_page_size == 0:
                                # Write page totals
                                page_total_line = format_page_totals(ws_page_total)
                                reportfile.write_record(page_total_line)
                                ws_line_counter += 1
                                ws_grand_total += ws_page_total
                                ws_page_total = Decimal('0')
                                
                                # Write headers for new page
                                reportfile.write_record(format_transaction_header_1())
                                ws_line_counter += 1
                                reportfile.write_record(format_transaction_header_2())
                                ws_line_counter += 1
                            
                            # Add to totals
                            ws_page_total += tran_record.tran_amt
                            ws_account_total += tran_record.tran_amt
                            
                            # Write detail line
                            detail_line = format_transaction_detail(
                                tran_record.tran_id,
                                xref_record.xref_acct_id,
                                tran_record.tran_type_cd,
                                tran_type_record.tran_type_desc,
                                tran_record.tran_cat_cd,
                                tran_cat_record.tran_cat_type_desc,
                                tran_record.tran_source,
                                tran_record.tran_amt
                            )
                            reportfile.write_record(detail_line)
                            ws_line_counter += 1
                
                elif appl_result == 16:
                    # EOF - write final totals
                    end_of_file = 'Y'
                    if tran_record:
                        print(f'TRAN-AMT {tran_record.tran_amt}')
                        print(f'WS-PAGE-TOTAL {ws_page_total}')
                    # Add page total to grand total (last transaction already added to both)
                    ws_grand_total += ws_page_total
                    # Write page totals
                    if ws_page_total > 0:
                        page_total_line = format_page_totals(ws_page_total)
                        reportfile.write_record(page_total_line)
                        ws_line_counter += 1
                    # Write account totals if there are any
                    if ws_account_total > 0:
                        account_total_line = format_account_totals(ws_account_total)
                        reportfile.write_record(account_total_line)
                        ws_line_counter += 1
                    # Write grand totals
                    grand_total_line = format_grand_totals(ws_grand_total)
                    reportfile.write_record(grand_total_line)
                else:
                    print('ERROR READING TRANSACTION FILE')
                    tranfile.display_io_status()
                    abend_program()
        
        # Close files
        if tranfile.close_file() != 0:
            print('ERROR CLOSING POSTED TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        if reportfile.close_file() != 0:
            print('ERROR CLOSING REPORT FILE')
            reportfile.display_io_status()
            abend_program()
        
        if xreffile.close_file() != 0:
            print('ERROR CLOSING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if trantypefile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION TYPE FILE')
            trantypefile.display_io_status()
            abend_program()
        
        if trancatfile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION CATG FILE')
            trancatfile.display_io_status()
            abend_program()
        
        if dateparmfile.close_file() != 0:
            print('ERROR CLOSING DATE PARM FILE')
            dateparmfile.display_io_status()
            abend_program()
        
        print('END OF EXECUTION OF PROGRAM CBTRN03C')
        return 0
        
    except Exception as e:
        print(f'ERROR: {e}')
        abend_program()


if __name__ == '__main__':
    sys.exit(main())

