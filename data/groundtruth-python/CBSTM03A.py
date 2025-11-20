"""
Ground Truth Python Equivalent for CBSTM03A.cbl

Program: CBSTM03A
Application: CardDemo
Type: BATCH Program
Function: Print Account Statements from Transaction data in two formats: 1/plain text and 2/HTML

Copyright Amazon.com, Inc. or its affiliates.
All Rights Reserved.
Licensed under the Apache License, Version 2.0
"""

import sys
from dataclasses import dataclass, field
from typing import Optional, List, Tuple
from decimal import Decimal


@dataclass
class CardXrefRecord:
    """CARD-XREF-RECORD from COPY CVACT03Y."""
    card_num: str = ""  # 16 chars
    cust_num: str = ""  # 9 chars
    acct_id: str = ""  # 11 chars
    
    @property
    def cust_id(self) -> str:
        return self.cust_num
    
    @property
    def xref_card_num(self) -> str:
        return self.card_num
    
    @property
    def xref_acct_id(self) -> str:
        return self.acct_id


@dataclass
class CustomerRecord:
    """CUSTOMER-RECORD from COPY CUSTREC."""
    cust_id: str = ""  # 9 chars
    first_name: str = ""
    middle_name: str = ""
    last_name: str = ""
    addr_line_1: str = ""
    addr_line_2: str = ""
    addr_line_3: str = ""
    addr_state_cd: str = ""
    addr_country_cd: str = ""
    addr_zip: str = ""
    fico_score: str = ""
    
    @property
    def cust_first_name(self) -> str:
        return self.first_name
    
    @property
    def cust_middle_name(self) -> str:
        return self.middle_name
    
    @property
    def cust_last_name(self) -> str:
        return self.last_name
    
    @property
    def cust_addr_line_1(self) -> str:
        return self.addr_line_1
    
    @property
    def cust_addr_line_2(self) -> str:
        return self.addr_line_2
    
    @property
    def cust_addr_line_3(self) -> str:
        return self.addr_line_3
    
    @property
    def cust_addr_state_cd(self) -> str:
        return self.addr_state_cd
    
    @property
    def cust_addr_country_cd(self) -> str:
        return self.addr_country_cd
    
    @property
    def cust_addr_zip(self) -> str:
        return self.addr_zip
    
    @property
    def cust_fico_credit_score(self) -> str:
        return self.fico_score


@dataclass
class AccountRecord:
    """ACCOUNT-RECORD from COPY CVACT01Y."""
    acct_id: str = ""  # 11 chars
    curr_bal: Decimal = Decimal('0')
    
    @property
    def acct_curr_bal(self) -> Decimal:
        return self.curr_bal


@dataclass
class TranRecord:
    """TRNX-RECORD from COPY COSTM01."""
    card_num: str = ""  # 16 chars
    trans_id: str = ""  # 16 chars
    trans_desc: str = ""
    trans_amt: Decimal = Decimal('0')
    trans_rest: str = ""  # remaining fields
    
    @property
    def trnx_card_num(self) -> str:
        return self.card_num
    
    @property
    def trnx_id(self) -> str:
        return self.trans_id
    
    @property
    def trnx_desc(self) -> str:
        return self.trans_desc
    
    @property
    def trnx_amt(self) -> Decimal:
        return self.trans_amt
    
    @property
    def trnx_rest(self) -> str:
        return self.trans_rest


class CBSTM03BHandler:
    """Handler for CBSTM03B subroutine calls."""
    
    def __init__(self):
        self.files: dict = {}
        self.file_handles: dict = {}
        self.current_positions: dict = {}
    
    def call_subroutine(self, dd_name: str, operation: str, key: str = "", key_len: int = 0) -> Tuple[str, str]:
        """Simulate CBSTM03B subroutine call.
        
        Returns: (return_code, field_data)
        """
        if operation == 'O':  # OPEN
            return self._open_file(dd_name)
        elif operation == 'C':  # CLOSE
            return self._close_file(dd_name)
        elif operation == 'R':  # READ (sequential)
            return self._read_sequential(dd_name)
        elif operation == 'K':  # READ-KEY (random)
            return self._read_key(dd_name, key, key_len)
        else:
            return ('12', '')
    
    def _open_file(self, dd_name: str) -> Tuple[str, str]:
        """Open a file."""
        try:
            filename = dd_name
            if dd_name == 'TRNXFILE':
                filename = 'TRNXFILE'
            elif dd_name == 'XREFFILE':
                filename = 'XREFFILE'
            elif dd_name == 'CUSTFILE':
                filename = 'CUSTFILE'
            elif dd_name == 'ACCTFILE':
                filename = 'ACCTFILE'
            
            # For Python, we'll use file-like objects
            # In real implementation, this would open actual files
            self.file_handles[dd_name] = open(filename, 'r', encoding='utf-8')
            self.current_positions[dd_name] = 0
            return ('00', '')
        except FileNotFoundError:
            return ('23', '')
        except Exception:
            return ('99', '')
    
    def _close_file(self, dd_name: str) -> Tuple[str, str]:
        """Close a file."""
        try:
            if dd_name in self.file_handles:
                self.file_handles[dd_name].close()
                del self.file_handles[dd_name]
            return ('00', '')
        except Exception:
            return ('99', '')
    
    def _read_sequential(self, dd_name: str) -> Tuple[str, str]:
        """Read next record sequentially."""
        try:
            if dd_name not in self.file_handles:
                return ('12', '')
            
            line = self.file_handles[dd_name].readline()
            if not line:
                return ('10', '')  # EOF
            
            return ('00', line.rstrip('\n\r'))
        except Exception:
            return ('99', '')
    
    def _read_key(self, dd_name: str, key: str, key_len: int) -> Tuple[str, str]:
        """Read record by key (simplified - would use indexed access in real implementation)."""
        # Simplified implementation - would need proper indexed file access
        try:
            if dd_name not in self.file_handles:
                return ('23', '')  # Not found
            
            # For now, return empty - would need proper key-based lookup
            return ('23', '')
        except Exception:
            return ('99', '')


def format_amount(amount: Decimal) -> str:
    """Format amount with sign indicator."""
    if amount < 0:
        return f"{abs(amount):9.2f}-"
    else:
        return f"{amount:9.2f} "


def parse_tran_record(line: str) -> TranRecord:
    """Parse transaction record from line."""
    if len(line) < 16:
        return TranRecord()
    
    return TranRecord(
        card_num=line[:16],
        trans_id=line[16:32] if len(line) > 16 else "",
        trans_desc=line[32:81] if len(line) > 32 else "",
        trans_amt=Decimal(line[81:96]) if len(line) > 81 and line[81:96].strip() else Decimal('0'),
        trans_rest=line[96:] if len(line) > 96 else ""
    )


def parse_xref_record(line: str) -> CardXrefRecord:
    """Parse XREF record from line."""
    if len(line) < 36:
        return CardXrefRecord()
    
    return CardXrefRecord(
        card_num=line[:16],
        cust_num=line[16:25],
        acct_id=line[25:36]
    )


def parse_customer_record(line: str) -> CustomerRecord:
    """Parse customer record from line."""
    # Simplified parsing - would need actual copybook structure
    parts = line.split('|')
    if len(parts) >= 10:
        return CustomerRecord(
            cust_id=parts[0][:9] if len(parts[0]) >= 9 else parts[0],
            first_name=parts[1] if len(parts) > 1 else "",
            middle_name=parts[2] if len(parts) > 2 else "",
            last_name=parts[3] if len(parts) > 3 else "",
            addr_line_1=parts[4] if len(parts) > 4 else "",
            addr_line_2=parts[5] if len(parts) > 5 else "",
            addr_line_3=parts[6] if len(parts) > 6 else "",
            addr_state_cd=parts[7] if len(parts) > 7 else "",
            addr_country_cd=parts[8] if len(parts) > 8 else "",
            addr_zip=parts[9] if len(parts) > 9 else "",
            fico_score=parts[10] if len(parts) > 10 else ""
        )
    return CustomerRecord()


def parse_account_record(line: str) -> AccountRecord:
    """Parse account record from line."""
    if len(line) < 11:
        return AccountRecord()
    
    acct_id = line[:11].strip()
    try:
        curr_bal = Decimal(line[11:26].strip()) if len(line) > 11 and line[11:26].strip() else Decimal('0')
    except:
        curr_bal = Decimal('0')
    
    return AccountRecord(acct_id=acct_id, curr_bal=curr_bal)


def abend_program():
    """Abend the program."""
    print('ABENDING PROGRAM')
    sys.exit(999)


def main():
    """Main procedure - equivalent to COBOL PROCEDURE DIVISION."""
    # Initialize files
    stmt_file = open('STMTFILE', 'w', encoding='utf-8')
    html_file = open('HTMLFILE', 'w', encoding='utf-8')
    
    # Initialize variables
    cr_cnt = 0  # COMP variable
    tr_cnt = 0  # COMP variable
    cr_jmp = 0  # COMP variable
    tr_jmp = 0  # COMP variable
    ws_total_amt = Decimal('0')  # COMP-3 variable
    ws_fl_dd = 'TRNXFILE'
    ws_trn_amt = Decimal('0')
    ws_save_card = ""
    end_of_file = 'N'
    
    # 2D array: WS-CARD-TBL[51][10] with WS-CARD-NUM[16] and WS-TRAN-NUM[16] + WS-TRAN-REST[318]
    ws_card_tbl: List[Tuple[str, List[Tuple[str, str]]]] = []
    ws_trn_tbl_ctr: List[int] = [0] * 51
    
    # Initialize arrays
    for i in range(51):
        ws_card_tbl.append(("", []))
    
    # M03B handler
    m03b_handler = CBSTM03BHandler()
    
    # Open files in sequence
    ws_m03b_dd = 'TRNXFILE'
    ws_m03b_oper = 'O'
    ws_m03b_rc, _ = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper)
    
    if ws_m03b_rc not in ('00', '04'):
        print('ERROR OPENING TRNXFILE')
        print(f'RETURN CODE: {ws_m03b_rc}')
        abend_program()
    
    # Read first transaction record
    ws_m03b_oper = 'R'
    ws_m03b_rc, ws_m03b_fldt = m03b_handler.call_subroutine('TRNXFILE', ws_m03b_oper)
    
    if ws_m03b_rc not in ('00', '04'):
        print('ERROR READING TRNXFILE')
        print(f'RETURN CODE: {ws_m03b_rc}')
        abend_program()
    
    trnx_record = parse_tran_record(ws_m03b_fldt)
    ws_save_card = trnx_record.trnx_card_num
    cr_cnt = 1
    tr_cnt = 0
    ws_fl_dd = 'READTRNX'
    
    # Read all transactions into table
    while True:
        if ws_save_card == trnx_record.trnx_card_num:
            tr_cnt += 1
        else:
            if cr_cnt > 0:
                ws_trn_tbl_ctr[cr_cnt - 1] = tr_cnt
            cr_cnt += 1
            tr_cnt = 1
        
        if cr_cnt <= 51:
            ws_card_tbl[cr_cnt - 1] = (trnx_record.trnx_card_num, [])
            if len(ws_card_tbl[cr_cnt - 1][1]) < tr_cnt:
                ws_card_tbl[cr_cnt - 1][1].extend([("", "")] * (tr_cnt - len(ws_card_tbl[cr_cnt - 1][1])))
            if tr_cnt <= 10:
                trans_list = list(ws_card_tbl[cr_cnt - 1][1])
                if len(trans_list) < tr_cnt:
                    trans_list.extend([("", "")] * (tr_cnt - len(trans_list)))
                trans_list[tr_cnt - 1] = (trnx_record.trnx_id, trnx_record.trnx_rest)
                ws_card_tbl[cr_cnt - 1] = (ws_card_tbl[cr_cnt - 1][0], trans_list)
        
        ws_save_card = trnx_record.trnx_card_num
        
        ws_m03b_dd = 'TRNXFILE'
        ws_m03b_oper = 'R'
        ws_m03b_rc, ws_m03b_fldt = m03b_handler.call_subroutine('TRNXFILE', ws_m03b_oper)
        
        if ws_m03b_rc == '00':
            trnx_record = parse_tran_record(ws_m03b_fldt)
            continue
        elif ws_m03b_rc == '10':
            break
        else:
            print('ERROR READING TRNXFILE')
            print(f'RETURN CODE: {ws_m03b_rc}')
            abend_program()
    
    if cr_cnt > 0:
        ws_trn_tbl_ctr[cr_cnt - 1] = tr_cnt
    
    # Open XREFFILE
    ws_m03b_dd = 'XREFFILE'
    ws_m03b_oper = 'O'
    ws_m03b_rc, _ = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper)
    
    if ws_m03b_rc not in ('00', '04'):
        print('ERROR OPENING XREFFILE')
        print(f'RETURN CODE: {ws_m03b_rc}')
        abend_program()
    
    # Open CUSTFILE
    ws_m03b_dd = 'CUSTFILE'
    ws_m03b_oper = 'O'
    ws_m03b_rc, _ = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper)
    
    if ws_m03b_rc not in ('00', '04'):
        print('ERROR OPENING CUSTFILE')
        print(f'RETURN CODE: {ws_m03b_rc}')
        abend_program()
    
    # Open ACCTFILE
    ws_m03b_dd = 'ACCTFILE'
    ws_m03b_oper = 'O'
    ws_m03b_rc, _ = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper)
    
    if ws_m03b_rc not in ('00', '04'):
        print('ERROR OPENING ACCTFILE')
        print(f'RETURN CODE: {ws_m03b_rc}')
        abend_program()
    
    # Main processing loop
    while end_of_file != 'Y':
        if end_of_file == 'N':
            # Read XREF record
            ws_m03b_dd = 'XREFFILE'
            ws_m03b_oper = 'R'
            ws_m03b_rc, ws_m03b_fldt = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper)
            
            if ws_m03b_rc == '00':
                card_xref_record = parse_xref_record(ws_m03b_fldt)
            elif ws_m03b_rc == '10':
                end_of_file = 'Y'
                continue
            else:
                print('ERROR READING XREFFILE')
                print(f'RETURN CODE: {ws_m03b_rc}')
                abend_program()
            
            if end_of_file == 'N':
                # Get customer record
                ws_m03b_dd = 'CUSTFILE'
                ws_m03b_oper = 'K'
                ws_m03b_key = card_xref_record.cust_id
                ws_m03b_key_ln = len(card_xref_record.cust_id)
                ws_m03b_rc, ws_m03b_fldt = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper, ws_m03b_key, ws_m03b_key_ln)
                
                if ws_m03b_rc == '00':
                    customer_record = parse_customer_record(ws_m03b_fldt)
                else:
                    print('ERROR READING CUSTFILE')
                    print(f'RETURN CODE: {ws_m03b_rc}')
                    abend_program()
                
                # Get account record
                ws_m03b_dd = 'ACCTFILE'
                ws_m03b_oper = 'K'
                ws_m03b_key = card_xref_record.xref_acct_id
                ws_m03b_key_ln = len(card_xref_record.xref_acct_id)
                ws_m03b_rc, ws_m03b_fldt = m03b_handler.call_subroutine(ws_m03b_dd, ws_m03b_oper, ws_m03b_key, ws_m03b_key_ln)
                
                if ws_m03b_rc == '00':
                    account_record = parse_account_record(ws_m03b_fldt)
                else:
                    print('ERROR READING ACCTFILE')
                    print(f'RETURN CODE: {ws_m03b_rc}')
                    abend_program()
                
                # Create statement
                _create_statement(stmt_file, html_file, customer_record, account_record, card_xref_record)
                
                cr_jmp = 1
                ws_total_amt = Decimal('0')
                
                # Process transactions
                for cr_idx in range(cr_cnt):
                    card_num, trans_list = ws_card_tbl[cr_idx]
                    if card_xref_record.xref_card_num == card_num:
                        for tr_idx in range(ws_trn_tbl_ctr[cr_idx]):
                            if tr_idx < len(trans_list):
                                trans_id, trans_rest = trans_list[tr_idx]
                                trnx_record = TranRecord(
                                    card_num=card_num,
                                    trans_id=trans_id,
                                    trans_rest=trans_rest
                                )
                                # Parse trans_rest to get desc and amt
                                if len(trans_rest) >= 50:
                                    trnx_record.trans_desc = trans_rest[:49]
                                    try:
                                        trnx_record.trans_amt = Decimal(trans_rest[49:64].strip()) if len(trans_rest) > 49 and trans_rest[49:64].strip() else Decimal('0')
                                    except:
                                        trnx_record.trans_amt = Decimal('0')
                                
                                _write_trans(stmt_file, html_file, trnx_record)
                                ws_total_amt += trnx_record.trnx_amt
                
                ws_trn_amt = ws_total_amt
                
                # Write total line
                stmt_file.write("-" * 80 + "\n")
                stmt_file.write(f"Total EXP:{'':56}${format_amount(ws_trn_amt)}\n")
                stmt_file.write("*" * 32 + "END OF STATEMENT" + "*" * 32 + "\n")
                
                # Write HTML footer
                html_file.write("<tr>\n")
                html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#1d1d96b3;">\n')
                html_file.write("<h3>End of Statement</h3>\n")
                html_file.write("</td>\n")
                html_file.write("</tr>\n")
                html_file.write("</table>\n")
                html_file.write("</body>\n")
                html_file.write("</html>\n")
    
    # Close files
    m03b_handler.call_subroutine('TRNXFILE', 'C')
    m03b_handler.call_subroutine('XREFFILE', 'C')
    m03b_handler.call_subroutine('CUSTFILE', 'C')
    m03b_handler.call_subroutine('ACCTFILE', 'C')
    
    stmt_file.close()
    html_file.close()


def _create_statement(stmt_file, html_file, customer_record: CustomerRecord, 
                     account_record: AccountRecord, xref_record: CardXrefRecord):
    """Create statement header."""
    # Write statement header
    stmt_file.write("*" * 31 + "START OF STATEMENT" + "*" * 31 + "\n")
    
    # Write HTML header
    html_file.write("<!DOCTYPE html>\n")
    html_file.write('<html lang="en">\n')
    html_file.write("<head>\n")
    html_file.write('<meta charset="utf-8">\n')
    html_file.write('<title>HTML Table Layout</title>\n')
    html_file.write("</head>\n")
    html_file.write('<body style="margin:0px;">\n')
    html_file.write('<table align="center" frame="box" style="width:70%; font:12px Segoe UI,sans-serif;">\n')
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#1d1d96b3;">\n')
    html_file.write(f'<h3>Statement for Account Number: {account_record.acct_id}</h3>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#FFAF33;">\n')
    html_file.write('<p style="font-size:16px">Bank of XYZ</p>\n')
    html_file.write('<p>410 Terry Ave N</p>\n')
    html_file.write('<p>Seattle WA 99999</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    
    # Customer name
    st_name = f"{customer_record.cust_first_name} {customer_record.cust_middle_name} {customer_record.cust_last_name}".strip()
    st_name = st_name[:75].ljust(75)
    stmt_file.write(st_name + "     \n")
    
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#f2f2f2;">\n')
    html_file.write(f'<p style="font-size:16px">{st_name.strip()}</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    
    # Address
    st_add1 = customer_record.cust_addr_line_1[:50].ljust(50)
    st_add2 = customer_record.cust_addr_line_2[:50].ljust(50)
    st_add3 = f"{customer_record.cust_addr_line_3} {customer_record.cust_addr_state_cd} {customer_record.cust_addr_country_cd} {customer_record.cust_addr_zip}".strip()[:80]
    
    stmt_file.write(st_add1 + "                              \n")
    stmt_file.write(st_add2 + "                              \n")
    stmt_file.write(st_add3 + "\n")
    
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#f2f2f2;">\n')
    html_file.write(f'<p>{st_add1.strip()}</p>\n')
    html_file.write(f'<p>{st_add2.strip()}</p>\n')
    html_file.write(f'<p>{st_add3}</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    
    # Account details
    stmt_file.write("-" * 80 + "\n")
    stmt_file.write(" " * 33 + "Basic Details" + " " * 33 + "\n")
    stmt_file.write("-" * 80 + "\n")
    stmt_file.write(f"Account ID         :{account_record.acct_id[:20]:20s}{'':40}\n")
    stmt_file.write(f"Current Balance    :{format_amount(account_record.acct_curr_bal)}{'':7}{'':40}\n")
    stmt_file.write(f"FICO Score         :{customer_record.cust_fico_credit_score[:20]:20s}{'':40}\n")
    stmt_file.write("-" * 80 + "\n")
    stmt_file.write(" " * 30 + "TRANSACTION SUMMARY " + " " * 30 + "\n")
    stmt_file.write("-" * 80 + "\n")
    stmt_file.write("Tran ID         Tran Details                                          Tran Amount\n")
    stmt_file.write("-" * 80 + "\n")
    
    # HTML account details
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#33FFD1; text-align:center;">\n')
    html_file.write('<p style="font-size:16px">Basic Details</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#f2f2f2;">\n')
    html_file.write(f'<p>Account ID         : {account_record.acct_id}</p>\n')
    html_file.write(f'<p>Current Balance    : {account_record.acct_curr_bal}</p>\n')
    html_file.write(f'<p>FICO Score         : {customer_record.cust_fico_credit_score}</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    html_file.write("<tr>\n")
    html_file.write('<td colspan="3" style="padding:0px 5px;background-color:#33FFD1; text-align:center;">\n')
    html_file.write('<p style="font-size:16px">Transaction Summary</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")
    html_file.write("<tr>\n")
    html_file.write('<td style="width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;">\n')
    html_file.write('<p style="font-size:16px">Tran ID</p>\n')
    html_file.write("</td>\n")
    html_file.write('<td style="width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;">\n')
    html_file.write('<p style="font-size:16px">Tran Details</p>\n')
    html_file.write("</td>\n")
    html_file.write('<td style="width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;">\n')
    html_file.write('<p style="font-size:16px">Amount</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")


def _write_trans(stmt_file, html_file, trnx_record: TranRecord):
    """Write transaction line."""
    stmt_file.write(f"{trnx_record.trnx_id[:16]:16s} {trnx_record.trnx_desc[:49]:49s}${format_amount(trnx_record.trnx_amt)}\n")
    
    html_file.write("<tr>\n")
    html_file.write('<td style="width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;">\n')
    html_file.write(f'<p>{trnx_record.trnx_id}</p>\n')
    html_file.write("</td>\n")
    html_file.write('<td style="width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;">\n')
    html_file.write(f'<p>{trnx_record.trnx_desc}</p>\n')
    html_file.write("</td>\n")
    html_file.write('<td style="width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;">\n')
    html_file.write(f'<p>{trnx_record.trnx_amt}</p>\n')
    html_file.write("</td>\n")
    html_file.write("</tr>\n")


if __name__ == '__main__':
    try:
        main()
    except Exception as e:
        print(f'ERROR: {e}')
        abend_program()

