def render(instr):
    if 'ops' in instr:
        optexts = []
        for op in instr['ops']:
            if isinstance(op, dict) and 'type' in op:
                optext = op['text'] if 'text' in op else hex(op['value'])
                optexts.append(optext)
            else:
                optexts.append(str(op))
        return "{} {}".format(instr['mnemonic'], ', '.join(optexts))
    else:
        return instr['mnemonic']

def sext(number, bit):
    mask = 1 << bit
    if number & mask > 0:
        return - ((~number & (mask - 1)) + 1)
    else:
        return number

NAMES = {
    0x00: "INDF0",
    0x01: "FSR0",
    0x02: "PCL",
    0x03: "PCLATH",
    0x04: "ALUSTA",
    0x05: "T0STA",
    0x06: "CPUSTA",
    0x07: "INTSTA",
    0x08: "INDF1",
    0x09: "FSR1",
    0x0a: "WREG",
    0x0b: "TMR0L",
    0x0c: "TMR0H",
    0x0d: "TBLPTRL",
    0x0e: "TBLPTRH",
    0x0f: "BSR",
    0x010: "PORTA",
    0x011: "DDRB",
    0x012: "PORTB",
    0x013: "RCSTA1",
    0x014: "RCREG1",
    0x015: "TXSTA1",
    0x016: "TXREG1",
    0x017: "SPRBG1",
    0x110: "DDRC",
    0x111: "PORTC",
    0x112: "DDRD",
    0x113: "PORTD",
    0x114: "DDRE",
    0x115: "PORTE",
    0x116: "PIR1",
    0x116: "PIE1",
    0x210: "TMR1",
    0x211: "TMR2",
    0x212: "TMR3L",
    0x213: "TMR3H",
    0x214: "PR1",
    0x215: "PR2",
    0x216: "PR3/CA1L",
    0x217: "PR3/CA1H",
    0x310: "PW1DCL",
    0x311: "PW2DCL",
    0x312: "PW1DCH",
    0x313: "PW2DCH",
    0x314: "CA2L",
    0x315: "CA2H",
    0x316: "TCON1",
    0x317: "TCON2",
    0x410: "PIR2",
    0x411: "PIE2",
    0x412: "Unimplemented",
    0x413: "RCSTA2",
    0x414: "RCREG2",
    0x415: "TXSTA2",
    0x416: "TXREG2",
    0x417: "SPBRG2",
    0x510: "DDRF",
    0x511: "PORTF",
    0x512: "DDRG",
    0x513: "PORTG",
    0x514: "ADCON0",
    0x515: "ADCON1",
    0x516: "ADRESL",
    0x517: "ADRESH",
    0x610: "SSPADD",
    0x611: "SSPCON1",
    0x612: "SSPCON2",
    0x613: "SSPSTAT",
    0x614: "SSPBUF",
    0x615: "Unimplemented",
    0x616: "Unimplemented",
    0x617: "Unimplemented",
    0x710: "PW3DCL",
    0x711: "PW3DCH",
    0x712: "CA3L",
    0x713: "CA3H",
    0x714: "CA4L",
    0x715: "CA4H",
    0x716: "TCON3",
    0x717: "Unimplemented",
    0x810: "DDRH",
    0x811: "PORTH",
    0x812: "DDRJ",
    0x813: "PORTJ",
    0x018: "PRODL",
    0x019: "PRODH"
}

def reg_name(number):
    if number in NAMES:
        return NAMES[number]
    else:
        return None

def disassemble(blob, offset):
    instr = {}
    instr['length'] = 1
    instr['mnemonic'] = "BAD"
    instrbytes = blob[offset:offset + 2]
    instrbytes.reverse()
#    instrbytes.reverse()
#    print("Decoding {:02x}{:02x}...".format(instrbytes[0], instrbytes[1]))

    if instrbytes[0] == 0x00:
        mnemonicmap = {
            0x00: "NOP",
            0x02: "RETURN",
            0x03: "SLEEP",
            0x04: "CLRWDT",
            0x05: "RETFIE"
        }
        instr['mnemonic'] = 'BAD'
        if instrbytes[1] in mnemonicmap:
            instr['mnemonic'] = mnemonicmap[instrbytes[1]]
    elif instrbytes[0] & 0xf0 < 0x30:
        d = instrbytes[0] & 0x01 == 0x01
        code = instrbytes[0] >> 1
        mnemonicmap = [
            "MOVWF",
            "SUBWFB",
            "SUBWF",
            "DECF",
            "IORWF",
            "ANDWF",
            "XORWF",
            "ADDWF",
            "ADDWFC",
            "COMF",
            "INCF",
            "DECSFZ",
            "RRCF",
            "RLCF",
            "SWAPF",
            "INCSFZ",
            "RRNCF",
            "RLNCF",
            "INFSNZ",
            "DCFSNZ",
            "CLRF",
            "SETF",
            "NEGW",
            "DAW"
        ]
        instr['mnemonic'] = mnemonicmap[code]
        instr['ops'] = [d, instrbytes[1]]
    elif instrbytes[0] & 0xf0 == 0x30:
        code = instrbytes[0] & 0x0f
        if code >= 4:
            instr['mnemonic'] = "BTG"
            bit = code & 0x7
            instr['ops'] = [bit, instrbytes[1]]
        else:
            mnemonicmap = [
                "CPFSLT",
                "CPFSEQ",
                "CPFSGT",
                "MULWF",
                "TSTFSZ",
                "<BAD>"
            ]
            instr['mnemonic'] = mnemonicmap[code]
            instr['ops'] = [instrbytes[1]]
    elif instrbytes[0] >= 0x80 and instrbytes[0] < 0xa0:
        mnemonicmap = [
            "BSF",
            "BCF",
            "BTFSS",
            "BTFSC"
        ]
        instr['mnemonic'] = mnemonicmap[(instrbytes[0] >> 3) & 0x3]
        bit = instrbytes[0] & 0x7
        instr['ops'] = [bit, instrbytes[1]]
    elif instrbytes[0] >= 0xa0 and instrbytes[0] < 0xa8:
        mnemonic = "???"
    elif instrbytes[0] >= 0xa8 and instrbytes[0] < 0xb0:
        mnemonic = "???"
    elif instrbytes[0] >= 0xb0 and instrbytes[0] < 0xc0:
        mnemonicmap = [
            "MOVLW",
            "ADDLW",
            "SUBLW",
            "IORLW",
            "XORLW",
            "ANDLW",
            "RETLW",
            "LCALL",
            "MOVLB",
            "BAD",
            "MOVLR_",
            "MOVLRx",
            "MULLW",
            "BAD",
            "BAD",
            "BAD"
        ]
        instr['mnemonic'] = mnemonicmap[instrbytes[0] & 0x0f]
        instr['ops'] = [instrbytes[1]]
    elif instrbytes[0] >= 0xc0 and instrbytes[0] < 0xe0:
        instr['mnemonic'] = "GOTO"
        instr['ops'] = [(instrbytes[0] & 0x1f) * 0x100 + instrbytes[1]]
    elif instrbytes[0] >= 0xe0 and instrbytes[0] <= 0xff:
        instr['mnemonic'] = "CALL"
        instr['ops'] = [(instrbytes[0] & 0x1f) * 0x100 + instrbytes[1]]
    return (offset + instr['length'] * 2, instr)
