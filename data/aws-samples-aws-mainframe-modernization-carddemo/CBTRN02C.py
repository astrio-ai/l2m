"""
Ground Truth Python Equivalent for CBTRN02C.cbl

Program: CBTRN02C
Application: CardDemo
Type: BATCH COBOL Program
Function: Post the records from daily transaction file.

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, Dict
from decimal import Decimal
from datetime import datetime


@dataclass
class DailyTranRecord:
    """Daily transaction record (DALYTRAN-RECORD from COPY CVTRA06Y)."""
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
    
    @property
    def dalytran_id(self) -> str:
        return self.tran_id
    
    @property
    def dalytran_type_cd(self) -> str:
        return self.tran_type_cd
    
    @property
    def dalytran_cat_cd(self) -> str:
        return self.tran_cat_cd
    
    @property
    def dalytran_source(self) -> str:
        return self.tran_source
    
    @property
    def dalytran_desc(self) -> str:
        return self.tran_desc
    
    @property
    def dalytran_amt(self) -> Decimal:
        return self.tran_amt
    
    @property
    def dalytran_merchant_id(self) -> str:
        return self.tran_merchant_id
    
    @property
    def dalytran_merchant_name(self) -> str:
        return self.tran_merchant_name
    
    @property
    def dalytran_merchant_city(self) -> str:
        return self.tran_merchant_city
    
    @property
    def dalytran_merchant_zip(self) -> str:
        return self.tran_merchant_zip
    
    @property
    def dalytran_card_num(self) -> str:
        return self.tran_card_num
    
    @property
    def dalytran_orig_ts(self) -> str:
        return self.tran_orig_ts


@dataclass
class TranRecord:
    """Transaction record (TRAN-RECORD from COPY CVTRA05Y)."""
    tran_id: str = ""
    tran_type_cd: str = ""
    tran_cat_cd: str = ""
    tran_source: str = ""
    tran_desc: str = ""
    tran_amt: Decimal = Decimal('0')
    tran_merchant_id: str = ""
    tran_merchant_name: str = ""
    tran_merchant_city: str = ""
    tran_merchant_zip: str = ""
    tran_card_num: str = ""
    tran_orig_ts: str = ""
    tran_proc_ts: str = ""
    
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
class AccountRecord:
    """Account record (ACCOUNT-RECORD from COPY CVACT01Y)."""
    acct_id: str = ""  # 11 chars
    curr_bal: Decimal = Decimal('0')
    curr_cyc_credit: Decimal = Decimal('0')
    curr_cyc_debit: Decimal = Decimal('0')
    credit_limit: Decimal = Decimal('0')
    expiration_date: str = ""  # Date string
    
    @property
    def acct_curr_bal(self) -> Decimal:
        return self.curr_bal
    
    @property
    def acct_curr_cyc_credit(self) -> Decimal:
        return self.curr_cyc_credit
    
    @property
    def acct_curr_cyc_debit(self) -> Decimal:
        return self.curr_cyc_debit
    
    @property
    def acct_credit_limit(self) -> Decimal:
        return self.credit_limit
    
    @property
    def acct_expiration_date(self) -> str:
        return self.expiration_date


@dataclass
class TranCatBalRecord:
    """Transaction category balance record (TRAN-CAT-BAL-RECORD from COPY CVTRA01Y)."""
    trancat_acct_id: str = ""  # 11 chars
    trancat_type_cd: str = ""  # 2 chars
    trancat_cd: str = ""  # 4 chars
    tran_cat_bal: Decimal = Decimal('0')
    
    @property
    def trancat_acct_id_prop(self) -> str:
        return self.trancat_acct_id
    
    @property
    def trancat_type_cd_prop(self) -> str:
        return self.trancat_type_cd
    
    @property
    def trancat_cd_prop(self) -> str:
        return self.trancat_cd
    
    @property
    def tran_cat_bal_prop(self) -> Decimal:
        return self.tran_cat_bal


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
            status_04 = f"{left}00{right}"
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
    
    def rewrite_record(self, key: str, record: str) -> int:
        """Rewrite/update a record."""
        if key in self._data:
            self._data[key] = record
            self.file_status = "00"
            return 0
        else:
            self.file_status = "23"
            return 12
    
    def write_record(self, key: str, record: str) -> int:
        """Write a new record."""
        self._data[key] = record
        self.file_status = "00"
        return 0
    
    def close_file(self) -> int:
        """Close file and save data."""
        try:
            with open(self.filename, "w", encoding='utf-8') as f:
                for key, value in sorted(self._data.items()):
                    f.write(value + '\n')
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
            status_04 = f"{left}00{right}"
            print(f'FILE STATUS IS: NNNN {status_04}')
    
    def _extract_key(self, line: str) -> str:
        """Extract key from line (override in subclasses)."""
        return line[:16].strip() if len(line) >= 16 else ""


class XrefFileHandler(IndexedFileHandler):
    """Handler for XREF file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract card number key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class AcctFileHandler(IndexedFileHandler):
    """Handler for ACCTFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract account ID key (11 chars)."""
        if len(line) >= 11:
            return line[:11].strip()
        return ""


class TranFileHandler(IndexedFileHandler):
    """Handler for TRANFILE."""
    
    def _extract_key(self, line: str) -> str:
        """Extract transaction ID key (16 chars)."""
        if len(line) >= 16:
            return line[:16]
        return ""


class TcatBalFileHandler(IndexedFileHandler):
    """Handler for TCATBALF file."""
    
    def _extract_key(self, line: str) -> str:
        """Extract composite key: acct_id (11) + type_cd (2) + cat_cd (4) = 17 chars."""
        if len(line) >= 17:
            return line[:17]
        return ""


def parse_daily_tran_record(line: str) -> DailyTranRecord:
    """Parse daily transaction record from line."""
    # Simplified parsing - would need actual copybook structure
    if len(line) < 16:
        return DailyTranRecord()
    
    # Assume format: ID(16) + type(2) + cat(4) + source(20) + desc(50) + amt(15) + merchant_id(10) + merchant_name(30) + merchant_city(20) + merchant_zip(10) + card(16) + orig_ts(26)
    # Total: 16+2+4+20+50+15+10+30+20+10+16+26 = 219 chars minimum
    record = DailyTranRecord()
    
    if len(line) >= 16:
        record.tran_id = line[0:16]
    if len(line) >= 18:
        record.tran_type_cd = line[16:18].strip()
    if len(line) >= 22:
        record.tran_cat_cd = line[18:22].strip()
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
    # Card number extraction - try to find it in the expected position
    # It might be at 177-192 or 178-193 depending on padding
    if len(line) >= 193:
        # Try position 177-192 first
        card_num = line[177:193].strip()
        if len(card_num) == 16 and card_num.isdigit():
            record.tran_card_num = card_num
        elif len(line) >= 194:
            # Try position 178-193
            card_num = line[178:194].strip()
            if len(card_num) == 16:
                record.tran_card_num = card_num
        # If still not found, try to extract from the line
        if not record.tran_card_num and len(line) > 177:
            # Look for 16 consecutive digits near the expected position
            for i in range(177, min(200, len(line) - 15)):
                candidate = line[i:i+16]
                if candidate.isdigit() and len(candidate) == 16:
                    record.tran_card_num = candidate
                    break
    if len(line) >= 219:
        record.tran_orig_ts = line[193:219]
    elif len(line) >= 193:
        # If timestamp is shorter, take what's available
        record.tran_orig_ts = line[193:]
    
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


def parse_account_record(line: str) -> AccountRecord:
    """Parse account record."""
    if len(line) < 11:
        return AccountRecord()
    
    acct_id = line[:11].strip()
    # Simplified parsing - would need actual copybook structure
    # Assume balance fields are in the data section
    parts = line[11:].split()
    curr_bal = Decimal('0')
    curr_cyc_credit = Decimal('0')
    curr_cyc_debit = Decimal('0')
    credit_limit = Decimal('10000')  # Default
    expiration_date = "9999-12-31"  # Default
    
    try:
        if len(parts) > 0:
            curr_bal = Decimal(parts[0])
        if len(parts) > 1:
            curr_cyc_credit = Decimal(parts[1])
        if len(parts) > 2:
            curr_cyc_debit = Decimal(parts[2])
        if len(parts) > 3:
            credit_limit = Decimal(parts[3])
        if len(parts) > 4:
            expiration_date = parts[4]
    except:
        pass
    
    return AccountRecord(
        acct_id=acct_id,
        curr_bal=curr_bal,
        curr_cyc_credit=curr_cyc_credit,
        curr_cyc_debit=curr_cyc_debit,
        credit_limit=credit_limit,
        expiration_date=expiration_date
    )


def parse_tran_cat_bal_record(line: str) -> TranCatBalRecord:
    """Parse transaction category balance record."""
    if len(line) < 17:
        return TranCatBalRecord()
    
    return TranCatBalRecord(
        trancat_acct_id=line[:11].strip(),
        trancat_type_cd=line[11:13],
        trancat_cd=line[13:17],
        tran_cat_bal=Decimal(line[17:32].strip()) if len(line) > 17 and line[17:32].strip() else Decimal('0')
    )


def format_db2_timestamp() -> str:
    """Generate DB2 format timestamp: YYYY-MM-DD-HH.MM.SS.MM0000"""
    now = datetime.now()
    return f"{now.year:04d}-{now.month:02d}-{now.day:02d}-{now.hour:02d}.{now.minute:02d}.{now.second:02d}.{now.microsecond//10000:02d}0000"


def format_record_for_write(record: object) -> str:
    """Format a record for writing to file."""
    # Simplified - would need actual record structure formatting
    if isinstance(record, TranRecord):
        return f"{record.tran_id:16s}{record.tran_type_cd:2s}{record.tran_cat_cd:4s}{record.tran_source:20s}{record.tran_desc:50s}{str(record.tran_amt):15s}{record.tran_merchant_id:10s}{record.tran_merchant_name:30s}{record.tran_merchant_city:20s}{record.tran_merchant_zip:10s}{record.tran_card_num:16s}{record.tran_orig_ts:26s}{record.tran_proc_ts:26s}"
    elif isinstance(record, TranCatBalRecord):
        return f"{record.trancat_acct_id:11s}{record.trancat_type_cd:2s}{record.trancat_cd:4s}{str(record.tran_cat_bal):15s}"
    elif isinstance(record, AccountRecord):
        return f"{record.acct_id:11s}{str(record.curr_bal):15s}{str(record.curr_cyc_credit):15s}{str(record.curr_cyc_debit):15s}{str(record.credit_limit):15s}{record.expiration_date:10s}"
    return ""


def abend_program():
    """Abend the program."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure equivalent to COBOL PROCEDURE DIVISION."""
    print('START OF EXECUTION OF PROGRAM CBTRN02C')
    
    # File handlers
    dalytran_file = SequentialFileHandler("DALYTRAN")
    tranfile = TranFileHandler("TRANFILE")
    xreffile = XrefFileHandler("XREFFILE")
    dalyrejs_file = SequentialFileHandler("DALYREJS")
    acctfile = AcctFileHandler("ACCTFILE")
    tcatbalfile = TcatBalFileHandler("TCATBALF")
    
    # Working variables
    end_of_file = 'N'
    transaction_count = 0
    reject_count = 0
    validation_fail_reason = 0
    validation_fail_reason_desc = ""
    create_trancat_rec = 'N'
    
    # Current records
    dalytran_record: Optional[DailyTranRecord] = None
    xref_record: Optional[CardXrefRecord] = None
    account_record: Optional[AccountRecord] = None
    tran_cat_bal_record: Optional[TranCatBalRecord] = None
    
    try:
        # Open files
        if dalytran_file.open_file("r") != 0:
            print('ERROR OPENING DALYTRAN')
            dalytran_file.display_io_status()
            abend_program()
        
        if tranfile.open_file("w") != 0:
            print('ERROR OPENING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        if xreffile.open_file("r") != 0:
            print('ERROR OPENING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if dalyrejs_file.open_file("w") != 0:
            print('ERROR OPENING DALY REJECTS FILE')
            dalyrejs_file.display_io_status()
            abend_program()
        
        if acctfile.open_file("r+") != 0:
            print('ERROR OPENING ACCOUNT MASTER FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tcatbalfile.open_file("r+") != 0:
            print('ERROR OPENING TRANSACTION BALANCE FILE')
            tcatbalfile.display_io_status()
            abend_program()
        
        # Main processing loop
        while end_of_file != 'Y':
            if end_of_file == 'N':
                # Read next daily transaction record
                line, appl_result = dalytran_file.read_next()
                
                if appl_result == 0 and line:
                    transaction_count += 1
                    dalytran_record = parse_daily_tran_record(line)
                    
                    # Validate transaction
                    validation_fail_reason = 0
                    validation_fail_reason_desc = ""
                    
                    # Lookup XREF
                    xref_key = dalytran_record.dalytran_card_num
                    xref_line, appl_result = xreffile.read_record(xref_key)
                    if appl_result == 0 and xref_line:
                        xref_record = parse_xref_record(xref_line)
                    else:
                        validation_fail_reason = 100
                        validation_fail_reason_desc = "INVALID CARD NUMBER FOUND"
                    
                    # If XREF found, lookup account
                    if validation_fail_reason == 0:
                        acct_key = xref_record.xref_acct_id
                        acct_line, appl_result = acctfile.read_record(acct_key)
                        if appl_result == 0 and acct_line:
                            account_record = parse_account_record(acct_line)
                            
                            # Check credit limit
                            temp_bal = account_record.acct_curr_cyc_credit - account_record.acct_curr_cyc_debit + dalytran_record.dalytran_amt
                            if account_record.acct_credit_limit < temp_bal:
                                validation_fail_reason = 102
                                validation_fail_reason_desc = "OVERLIMIT TRANSACTION"
                            
                            # Check expiration date
                            if validation_fail_reason == 0:
                                if len(dalytran_record.dalytran_orig_ts) >= 10:
                                    tran_date = dalytran_record.dalytran_orig_ts[:10]
                                    # Compare dates (format: YYYY-MM-DD)
                                    if account_record.acct_expiration_date and tran_date:
                                        if account_record.acct_expiration_date < tran_date:
                                            validation_fail_reason = 103
                                            validation_fail_reason_desc = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
                        else:
                            validation_fail_reason = 101
                            validation_fail_reason_desc = "ACCOUNT RECORD NOT FOUND"
                    
                    # Post transaction or reject
                    if validation_fail_reason == 0:
                        # Post transaction
                        tran_record = TranRecord(
                            tran_id=dalytran_record.dalytran_id,
                            tran_type_cd=dalytran_record.dalytran_type_cd,
                            tran_cat_cd=dalytran_record.dalytran_cat_cd,
                            tran_source=dalytran_record.dalytran_source,
                            tran_desc=dalytran_record.dalytran_desc,
                            tran_amt=dalytran_record.dalytran_amt,
                            tran_merchant_id=dalytran_record.dalytran_merchant_id,
                            tran_merchant_name=dalytran_record.dalytran_merchant_name,
                            tran_merchant_city=dalytran_record.dalytran_merchant_city,
                            tran_merchant_zip=dalytran_record.dalytran_merchant_zip,
                            tran_card_num=dalytran_record.dalytran_card_num,
                            tran_orig_ts=dalytran_record.dalytran_orig_ts,
                            tran_proc_ts=format_db2_timestamp()
                        )
                        
                        # Update transaction category balance
                        tcatbal_key = f"{xref_record.xref_acct_id:11s}{dalytran_record.dalytran_type_cd:2s}{dalytran_record.dalytran_cat_cd:4s}"
                        tcatbal_line, appl_result = tcatbalfile.read_record(tcatbal_key)
                        
                        if appl_result == 0 and tcatbal_line:
                            tran_cat_bal_record = parse_tran_cat_bal_record(tcatbal_line)
                            tran_cat_bal_record.tran_cat_bal += dalytran_record.dalytran_amt
                            tcatbal_record_str = format_record_for_write(tran_cat_bal_record)
                            tcatbalfile.rewrite_record(tcatbal_key, tcatbal_record_str)
                        else:
                            # Create new record
                            tran_cat_bal_record = TranCatBalRecord(
                                trancat_acct_id=xref_record.xref_acct_id,
                                trancat_type_cd=dalytran_record.dalytran_type_cd,
                                trancat_cd=dalytran_record.dalytran_cat_cd,
                                tran_cat_bal=dalytran_record.dalytran_amt
                            )
                            tcatbal_record_str = format_record_for_write(tran_cat_bal_record)
                            tcatbalfile.write_record(tcatbal_key, tcatbal_record_str)
                        
                        # Update account record
                        account_record.curr_bal += dalytran_record.dalytran_amt
                        if dalytran_record.dalytran_amt >= 0:
                            account_record.curr_cyc_credit += dalytran_record.dalytran_amt
                        else:
                            account_record.curr_cyc_debit += dalytran_record.dalytran_amt
                        
                        acct_record_str = format_record_for_write(account_record)
                        acctfile.rewrite_record(acct_key, acct_record_str)
                        
                        # Write transaction record
                        tran_record_str = format_record_for_write(tran_record)
                        tran_key = tran_record.tran_id
                        tranfile.write_record(tran_key, tran_record_str)
                    else:
                        # Write reject record
                        reject_count += 1
                        reject_record = f"{line[:350]:350s}{validation_fail_reason:04d}{validation_fail_reason_desc:76s}"
                        dalyrejs_file.write_record(reject_record)
                
                elif appl_result == 16:
                    end_of_file = 'Y'
                else:
                    print('ERROR READING DALYTRAN FILE')
                    dalytran_file.display_io_status()
                    abend_program()
        
        # Close files
        if dalytran_file.close_file() != 0:
            print('ERROR CLOSING DALYTRAN FILE')
            dalytran_file.display_io_status()
            abend_program()
        
        if tranfile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION FILE')
            tranfile.display_io_status()
            abend_program()
        
        if xreffile.close_file() != 0:
            print('ERROR CLOSING CROSS REF FILE')
            xreffile.display_io_status()
            abend_program()
        
        if dalyrejs_file.close_file() != 0:
            print('ERROR CLOSING DAILY REJECTS FILE')
            dalyrejs_file.display_io_status()
            abend_program()
        
        if acctfile.close_file() != 0:
            print('ERROR CLOSING ACCOUNT FILE')
            acctfile.display_io_status()
            abend_program()
        
        if tcatbalfile.close_file() != 0:
            print('ERROR CLOSING TRANSACTION BALANCE FILE')
            tcatbalfile.display_io_status()
            abend_program()
        
        print(f'TRANSACTIONS PROCESSED : {transaction_count}')
        print(f'TRANSACTIONS REJECTED  : {reject_count}')
        
        print('END OF EXECUTION OF PROGRAM CBTRN02C')
        
        if reject_count > 0:
            return 4
        return 0
        
    except Exception as e:
        print(f'ERROR: {e}')
        abend_program()


if __name__ == '__main__':
    sys.exit(main())

