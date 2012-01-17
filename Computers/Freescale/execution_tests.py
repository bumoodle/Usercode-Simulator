#!/usr/bin/python2

import sys, inspect
sys.path.append('../..')

import unittest

from HCS08 import MC9S08QG4 as uProc

#these constants allow easy creation of dictionary keys
A = 'A'
X = 'X'
H = 'H'
PC = 'PC'
SP = 'SP'
C = 'C'
HC = 'HC'
N = 'N'
Z = 'Z'
I = 'I'
V = 'V'


def test_ADC():

    # ***Step 1: Create a new CPU, preloaded with your program.
    cpu = uProc(["ADC #$0F", "ADC #$FF", "ADC #$01", "ADC #$7F"])

    #  ***Step 2: Set up the CPU however you'd like. Some notes that may help:
    #
    # -You can access registers and flags directly ("cpu.A = 12", "cpu.C = True")
    # -The ram is a dictionary ("associative array" or set of name/value pairs) which maps an address to a value.
    #  You can access address $FF via cpu.ram[0xFF], like so "cpu.ram[0xFF] = 13".
    # -The flash is organized the same way, but is called cpu.flash.
    #
    # For more information, view the contents of HCS08.py.

    cpu.A = 0x0F
    cpu.C = True

    # ***Step 3: Run the instruction (or instructions). In this case, we'll single step, executing the first instruction specified.
    cpu.step()

    # ***Step 4: Verify that the instruction acted as expected.
    assert cpu.A == 0x1F, "ADC instruction did not set A correctly. Should have been 31, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."

    cpu.A = 0x01
    cpu.C = False
    cpu.step()
    assert cpu.A == 0x00, "ADC instruction did not set A correctly. Should have been 0, was " + repr(cpu.A)
    assert cpu.C == True, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == True, "Z was set inappropriately."

    cpu.A = 0x01
    cpu.C = True
    cpu.step()
    assert cpu.A == 0x03, "ADC instruction did not set A correctly. Should have been 3, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.HC == False, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."

    cpu.A = 0x7F
    cpu.C = True
    cpu.step()
    assert cpu.A == 0xFF, "ADC instruction did not set A correctly. Should have been 255, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == True, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == True, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."



def test_ADD():
    cpu = uProc(["ADD #$0F", "ADD #$FF", "ADD #$7F"])

    cpu.A = 0x0F
    cpu.step()
    assert cpu.A == 0x1E, "ADD instruction did not set A correctly. Should have been 30, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."

    cpu.A = 0x01
    cpu.step()
    assert cpu.A == 0x00, "ADD instruction did not set A correctly. Should have been 0, was " + repr(cpu.A)
    assert cpu.C == True, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == True, "Z was set inappropriately."

    cpu.A = 0x7F
    cpu.step()
    assert cpu.A == 0xFE, "ADD instruction did not set A correctly. Should have been 254, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == True, "V was set inappropriately."
    assert cpu.HC == True, "H was set inappropriately."
    assert cpu.N == True, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."



def test_AIS():
    cpu = uProc("AIS #$01")

    cpu.SP = 0x00
    cpu.step()
    assert cpu.SP == 0x01, "AIS instruction did not set S correctly. Should have been 1, was " + repr(cpu.A)



def test_AIX():
    cpu = uProc(["AIX #$01", "AIX #$FF"])

    cpu.H = 0x00
    cpu.X = 0x00
    cpu.step()
    assert cpu.H == 0x00, "AIX instruction did not set H correctly. Should have been 0, was " + repr(cpu.A)
    assert cpu.X == 0x01, "AIX instruction did not set X correctly. Should have been 1, was " + repr(cpu.A)

    cpu.H = 0x00
    cpu.X = 0x01
    cpu.step()
    assert cpu.H == 0x01, "AIX instruction did not set H correctly. Should have been 1, was " + repr(cpu.A)
    assert cpu.X == 0x00, "AIX instruction did not set X correctly. Should have been 0, was " + repr(cpu.A)



def test_AND():
    cpu = uProc(["AND #$0F", "AND #$FF", "AND #$00"])

    cpu.A = 0xF7
    cpu.step()
    assert cpu.A == 0x07, "AND instruction did not set A correctly. Should have been 7, was " + repr(cpu.A)
    assert cpu.N == False, "N was set inappropriately."
    assert cpu.Z == False, "Z was set inappropriately."

    cpu.A = 0xF7
    cpu.step()
    assert cpu.A == 0xF7, "AND instruction did not set A correctly. Should have been 247, was " + repr(cpu.A)
    assert cpu.N == True, "N was set inappropriately."
    assert cpu.Z == False, "Z was set inappropriately."

    cpu.A = 0xF7
    cpu.step()
    assert cpu.A == 0x00, "AND instruction did not set A correctly. Should have been 0, was " + repr(cpu.A)
    assert cpu.N == False, "N was set inappropriately."
    assert cpu.Z == True, "Z was set inappropriately."



def test_ASL():
    cpu = uProc(["ASLA", "ASLA", "ASLA", "ASLA"])

    cpu.A = 0x01
    cpu.step()
    massassert(cpu, A=2, C=0, N=0, Z=0, V=0)

    cpu.A = 0x78
    cpu.step()
    massassert(cpu, A=0xF0, C=0, N=1, Z=0, V=1)

    cpu.A = 0x00
    cpu.step()
    massassert(cpu, A=0, C=0,  N=0, Z=1, V=0)

    cpu.A = 0xF0
    cpu.step()
    massassert(cpu, A=0xE0, C=1, N=1, Z=0, V=0)


def test_ASR():
    cpu = uProc(["ASRA", "ASRA", "ASRA", "ASRA"])

    cpu.A = 0x01
    cpu.step()
    massassert(cpu, A=0, C=1, N=0, Z=1, V=1)

    cpu.A = 0x1E
    cpu.step()
    massassert(cpu, A=0x0F, C=0, N=0, Z=0, V=0)

    cpu.A = 0x80
    cpu.step()
    massassert(cpu, A=0xC0, C=0,  N=1, Z=0, V=1)

    cpu.A = 0xF1
    cpu.step()
    massassert(cpu, A=0xF8, C=1, N=1, Z=0, V=0)


def test_BCC():
    cpu = uProc(["BCC 5", "BCC 5"])

    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BCLR():
    cpu = uProc(["BCLR 0,5"])

    cpu.ram[0x05] = 0xFF
    cpu.step()
    assert cpu.ram[0x05] == 0xFE, "Incorrect RAM value- should have been $FE, was " + repr(cpu.ram[0x05])




def test_BCS():
    cpu = uProc(["BCS 5", "BCS 5"])

    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BEQ():
    cpu = uProc(["BEQ 5", "BEQ 5"])

    cpu.Z = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.Z = 1
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BGE():
    cpu = uProc(["BGE 5", "BGE 5"])

    cpu.N = 1
    cpu.V = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.N = 0
    cpu.V = 0
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BGT():
    cpu = uProc(["BGT 4", "BGT 4", "BGT 4"])

    cpu.N = 0
    cpu.V = 0
    cpu.Z = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.N = 1
    cpu.V = 1
    cpu.Z = 0
    cpu.step()
    massassert(cpu, PC=0xF008)

    cpu.PC = 0xF000
    cpu.N = 1
    cpu.V = 0
    cpu.Z = 0
    cpu.step()
    massassert(cpu, PC=0xF002)



def test_BHCC():
    cpu = uProc(["BHCC 5", "BHCC 5"])

    cpu.HC = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.HC = 0
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BHCS():
    cpu = uProc(["BHCS 5", "BHCS 5"])

    cpu.HC = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.HC = 1
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BHI():
    cpu = uProc(["BHI 5"])

    cpu.Z = 0
    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF007)

    cpu.PC = 0xF000
    cpu.Z = 1
    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.PC = 0xF000
    cpu.Z = 0
    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.PC = 0xF000
    cpu.Z = 1
    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF002)



def test_BHS():
    cpu = uProc(["BHS 5", "BHS 5"])

    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BIT():
    cpu = uProc(["BIT #$0F", "BIT #$FF", "BIT #$00"])

    cpu.A = 0xF0
    cpu.step()
    massassert(cpu, A=0xF0, N=0, Z=1)

    cpu.step()
    massassert(cpu, A=0xF0, N=1, Z=0)

    cpu.step()
    massassert(cpu, A=0xF0, N=0, Z=1)



def test_BLE():
    cpu = uProc(["BLE 4", "BLE 4", "BLE 4"])

    cpu.N = 1
    cpu.V = 1
    cpu.Z = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.N = 0
    cpu.V = 0
    cpu.Z = 1
    cpu.step()
    massassert(cpu, PC=0xF008)

    cpu.PC = 0xF000
    cpu.N = 1
    cpu.V = 0
    cpu.Z = 0
    cpu.step()
    massassert(cpu, PC=0xF006)



def test_BLO():
    cpu = uProc(["BLO 5", "BLO 5"])

    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.C = 1
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BLS():
    cpu = uProc(["BLS 5", "BLS 5"])

    cpu.Z = 0
    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.Z = 0
    cpu.C = 1
    massset(cpu, Z=0, C=1)

    cpu.step()
    massassert(cpu, PC=0xF009)

    cpu.PC = 0xF002
    cpu.Z = 1
    cpu.C = 0
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BLT():
    cpu = uProc(["BLT 5", "BLT 5"])

    cpu.N = 0
    cpu.V = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.N = 0
    cpu.V = 1
    cpu.step()
    massassert(cpu, PC=0xF009)

    cpu.PC = 0xF002
    cpu.N = 1
    cpu.V = 0
    cpu.step()
    massassert(cpu, PC=0xF009)

    cpu.PC = 0xF002
    cpu.N = 1
    cpu.V = 1
    cpu.step()
    massassert(cpu, PC=0xF004)



def test_BMC():
    cpu = uProc(["BMC 5", "BMC 5"])

    cpu.I = 1
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.I = 0
    cpu.step()
    massassert(cpu, PC=0xF009)


def test_BMS():
    cpu = uProc(["BMS 5", "BMS 5"])

    cpu.I = 0
    cpu.step()
    massassert(cpu, PC=0xF002)

    cpu.I = 1
    cpu.step()
    massassert(cpu, PC=0xF009)



def test_BNE():
    automultitest(["BNE 5"]*2, ({Z:1}, {PC:0xF002}), ({Z:0}, {PC:0xF009}))



def test_BPL():
    automultitest(["BPL 5"]*2, ({N:1}, {PC:0xF002}), ({N:0}, {PC:0xF009}))


def test_BRA():
    autotest(["BRA 5"], {}, {PC:0xF007})


def test_BRN():
    autotest(["BRN 5"], {}, {PC:0xF002})


def test_BRCLR():
    automultitest(["BRCLR 0,5,5"]*2, ({5:0xFF}, {PC:0xF003}), ({5:0x00}, {PC: 0xF00B}))


def test_BRSET():
    automultitest(["BRSET 0,5,5"]*2, ({5:0x00}, {PC:0xF003}), ({5:0xFF}, {PC: 0xF00B}))


def test_BSET():
    cpu = uProc(["BSET 0,5"])

    cpu.ram[0x05] = 0x00
    cpu.step()
    assert_by_dict(cpu, {0x05:0x01})


def test_BSR():
    autotest(["BSR 5"], {SP:0x09}, {PC:0xF007, SP:0x07, 9:0x02, 8:0xF0})


def test_CBEQ():
    amt(["CBEQA #3,5"]*2, ({A:0}, {PC:0xF003}), ({A:3}, {PC:0xF00B}))


def test_CLC():
    autotest(["CLC"], {C:1}, {C:0})


def test_CLI():
    autotest(["CLI"], {I:1}, {I:0})


def test_CLR():
    autotest(["CLRA"], {A:0xFF}, {A:0})


def test_CMP(): #Z, V, N, C
    amt(["CMP #5", "CMP #1", "CMP #1", "CMP #$80"], ({A:5},{Z:1, A:5}),({A:0x80},{V:1}),({A:0},{N:1}),({A:0},{C:1}))


def test_COM():
    autotest(["COMA"], {A:0xFF}, {A:0x00, V:0, N:0, Z:1, C:1})


def test_CPHX():
    amt(["CPHX #5", "CPHX #1", "CPHX #1", "CPHX #$8000"], ({H:0, X:5},{Z:1, H:0, X:5}), ({H:0x80, X:0},{V:1}), ({H:0,X:0},{N:1}), ({H:0, X:0}, {C:1}))


def test_CPX(): #Z, V, N, C
    amt(["CPX #5", "CPX #1", "CPX #1", "CPX #$80"], ({X:5},{Z:1, X:5}),({X:0x80},{V:1}),({X:0},{N:1}),({X:0},{C:1}))


def test_DAA():
    amt(["DAA"]*3, ({A:0x55,HC:0,C:0},{A:0x55,C:0}), ({A:0x5D,C:0,HC:0},{A:0x63,C:0}), ({A:0xD5,C:0,H:0},{A:0x65, C:1}))
    amt(["DAA"]*3, ({A:0x52,HC:1,C:0},{A:0x58,C:0}), ({A:0xA2,C:0,HC:1},{A:0x02,C:1}), ({A:0x2F,C:1,H:1},{A:0x95, C:1}))


def test_DBNZ():
    amt(["DBNZA 5"]*2, ({A:1},{A:0,PC:0xF002}),({A:6},{A:5,PC:0xF009}))


def test_DEC():
    amt(["DECA"]*3, ({A:1},{A:0,Z:1}),({A:0},{A:0xFF,N:1}),({A:0x80},{A:0x7F,V:1}))


def test_DIV():
    amt(["DIV"]*3, ({H:0,A:0,X:5},{A:0,Z:1,H:0}), ({H:0,A:5,X:0},{C:1}), ({H:0,A:7,X:3},{A:2,H:1}))


def test_EOR():
    amt(["EOR #$F0"]*2, ({A:0xF0,V:1},{A:0,Z:0,V:0}), ({A:0x00},{A:0xF0,N:1,V:0}))


def test_INC():
    amt(["INCA"]*3, ({A:0xFF},{A:0,Z:1}),({A:0x80},{A:0x81,N:1}),({A:0x7F},{A:0x80,V:1}))


def test_JMP():
    autotest(["JMP 5"], {}, {PC:0x05})


def test_JSR():
    autotest(["JSR 5"], {SP:0x09}, {PC:0xF007, SP:0x07, 9:0x02, 8:0xF0})
    autotest(["JSR $F150"], {SP:0x09}, {PC:0xF150, SP:0x07, 9:0x03, 8:0xF0})


def test_LDA():
    amt(["LDA #3", "LDA #0", "LDA #$FF"], ({},{A:3,V:0,N:0,Z:0}), ({},{A:0,V:0,N:0,Z:1}), ({},{A:0xFF,V:0,N:1,Z:0}))


def test_LDHX():
    amt(["LDHX #3", "LDHX #0", "LDHX #$FFFF"], ({},{H:0,X:3,V:0,N:0,Z:0}), ({},{H:0,X:0,V:0,N:0,Z:1}), ({},{H:0xFF,X:0xFF,V:0,N:1,Z:0}))


def test_LDX():
    amt(["LDX #3", "LDX #0", "LDX #$FF"], ({},{X:3,V:0,N:0,Z:0}), ({},{X:0,V:0,N:0,Z:1}), ({},{X:0xFF,V:0,N:1,Z:0}))


def test_LSL():
    cpu = uProc(["LSLA", "LSLA", "LSLA", "LSLA"])

    cpu.A = 0x01
    cpu.step()
    massassert(cpu, A=2, C=0, N=0, Z=0, V=0)

    cpu.A = 0x78
    cpu.step()
    massassert(cpu, A=0xF0, C=0, N=1, Z=0, V=1)

    cpu.A = 0x00
    cpu.step()
    massassert(cpu, A=0, C=0,  N=0, Z=1, V=0)

    cpu.A = 0xF0
    cpu.step()
    massassert(cpu, A=0xE0, C=1, N=1, Z=0, V=0)


def test_LSR():
    cpu = uProc(["LSRA", "LSRA", "LSRA", "LSRA"])

    cpu.A = 0x01
    cpu.step()
    massassert(cpu, A=0, C=1, N=0, Z=1, V=1)

    cpu.A = 0x1E
    cpu.step()
    massassert(cpu, A=0x0F, C=0, N=0, Z=0, V=0)

    cpu.A = 0x80
    cpu.step()
    massassert(cpu, A=0x40, C=0,  N=0, Z=0, V=0)

    cpu.A = 0xF1
    cpu.step()
    massassert(cpu, A=0x78, C=1, N=0, Z=0, V=1)


def test_MOV():
    amt(["MOV #$FF,5", "MOV 5,6"], ({5:3, 6:0}, {5:0xFF, 6:0, V:0, N:1, Z:0}), ({5:0xFF, 6:0}, {5:0, 6:0, V:0, N:0, Z:1}))


def test_MUL():
    amt(["MUL"]*2, ({X:3,A:2},{X:0,A:6}), ({X:0x80,A:2},{X:01,A:0}))


def test_NEG():
    amt(["NEGA"]*3, ({A:0},{A:0,Z:1,C:0,V:0,N:0}), ({A:0x80},{A:0x80,Z:0,C:1,V:1,N:1}), ({A:1},{A:0xFF,Z:0,C:1,V:0,N:1}))


def test_NSA():
    autotest(["NSA"], {A:0xF0}, {A:0x0F})


def test_ORA():
    amt(["ORA #$0F", "ORA #$F0", "ORA #0"], ({A:5},{A:0x0F,V:0,N:0,Z:0}),({A:5},{A:0xF5,V:0,N:1,Z:0}),({A:0},{A:0,V:0,N:0,Z:1}))


def test_PSHA():
    autotest(["PSHA"], {A:5,SP:0x09,0x09:8}, {A:5, SP:0x08, 0x09:5})


def test_PSHH():
    autotest(["PSHH"], {H:5,SP:0x09,0x09:8}, {H:5, SP:0x08, 0x09:5})


def test_PSHX():
    autotest(["PSHX"], {X:5,SP:0x09,0x09:8}, {X:5, SP:0x08, 0x09:5})


def test_PULA():
    autotest(["PULA"], {A:6,SP:0x08,0x09:8}, {A:8, SP:0x09})


def test_PULH():
    autotest(["PULH"], {H:6,SP:0x08,0x09:8}, {H:8, SP:0x09})


def test_PULX():
    autotest(["PULX"], {X:6,SP:0x08,0x09:8}, {X:8, SP:0x09})


def test_ROL():
    amt(["ROLA"]*3, ({A:1,C:1},{A:3,C:0,V:0,N:0,Z:0}), ({A:0xF0,C:0},{A:0,C:1,V:1,N:0,Z:1}), ({A:0x78,C:1},{A:0xF1,C:1,V:0,N:1,Z:0}))


def test_ROR():
    amt(["RORA"]*3, ({A:1,C:1},{A:0x80,C:1,N:1,Z:0,V:0}), ({A:1,C:0},{A:0,C:1,N:0,Z:1,V:1}), ({A:9,C:0},{A:4,C:1,Z:0,N:0,V:1}))


def test_RSP():
    autotest(["RSP"], {SP:0xF150}, {SP:0xF1FF})


def test_RTI():
    autotest(["RTI"], {SP:0xF100, V:0, H:0, I:0, N:0, Z:0, C:0, A:5, X:5, 0xF101:0xFF, 0xF102:9, 0xF103:7, 0xF104:0xF0, 0xF105:2}, {SP:0xF105, V:1,H:1,I:1,N:1,Z:1,C:1, A:9, X:7, PC:0xF002})


def test_RTS():
    autotest(["RTS"], {SP:0xF100, 0xf101:0xF4, 0xf102:0x06}, {SP:0xF102, PC:0xF406})


def test_SBC():
    amt(["SBC #1"]*4, ({A:2,C:1},{A:0,N:0,Z:1,C:0,V:0}), ({A:0x80,C:0},{A:0x7F,N:0,Z:0,C:0,V:1}), ({A:0,C:1},{A:0xFE,N:1,Z:0,C:1,V:0}))


def test_SEC():
    autotest(["SEC"], {C:0}, {C:1})


def test_SEI():
    autotest(["SEI"], {I:0}, {I:1})


def test_STA():
    autotest(["STA 6"], {A:9, 6:2}, {A:9, 6:9, V:0, N:0, Z:0})


def test_STHX():
    autotest(["STHX 9"], {H:1,X:3,5:9,6:9}, {H:1,X:3,5:1,6:3,V:0,N:0,Z:0})


def test_STX():
    autotest(["STX 6"], {X:9, 6:2}, {X:9, 6:9, V:0, N:0, Z:0})


def test_SUB():
    amt(["SUB #1"]*4, ({A:1},{A:0,N:0,Z:1,C:0,V:0}), ({A:0x80},{A:0x7F,N:0,Z:0,C:0,V:1}), ({A:0},{A:0xFF,N:1,Z:0,C:1,V:0}))


def test_SWI():
    autotest(["SWI"], {V:1,H:0,I:0,N:0,Z:0,C:0, A:8, X:3, SP:0xF108, 0xFFFC:9, 0xFFFD:0}, {PC:0x0900, 0xF108:1, 0xF107:0xF0, 0xF106:3, 0xF105:8, 0xF104:0xE0, I:1})


def test_TAP():
    autotest(["TAP"], {V:0,H:0,I:0,N:0,Z:0,C:0, A:0x8F}, {V:1,H:0,I:1,N:1,Z:1,C:1})


def test_TAX():
    autotest(["TAX"], {A:3,X:8}, {A:3,X:3})


def test_TPA():
    autotest(["TPA"], {V:1,H:1,I:0,N:0,Z:0,C:1, A:0x00}, {V:1,H:1,I:0,N:0,Z:0,C:1,A:0xF1})


def test_TST():
    amt(["TSTA"]*3, ({A:0,V:1,Z:0,N:1},{A:0,V:0,Z:1,N:0}),({A:0x80},{A:0x80,V:0,N:1,Z:0}),({A:5},{A:5,V:0,N:0,Z:0}))


def test_TSX():
    autotest(["TSX"], {SP:0xF080,H:0,X:0}, {SP:0xF080,H:0xF0,X:0x81})


def test_TXA():
    autotest(["TXA"], {A:3,X:8}, {A:8,X:8})


def test_TXS():
    autotest(["TSX"], {SP:0x0000,H:0xF0,X:0x81}, {SP:0xF080,H:0xF0,X:0x81})





#def amt(c, *s):
#    automultitest(c, *s)


def automultitest(code, *saps):

    cpu = uProc(code)

    for sets, asserts in saps:
            set_by_dict(cpu, sets)
            cpu.step()
            assert_by_dict(cpu, asserts)

#shorthand
amt = automultitest


def autotest(code, sets, asserts):
    """
        Runs a single common test case automatically.
    """

    #create a new CPU
    cpu = uProc(code)

    #perform set-up
    set_by_dict(cpu, sets)

    #run the instruction
    cpu.step()

    #and then test the assumptions
    assert_by_dict(cpu, asserts)


def set_by_dict(cpu, dictionary):
    """
        Sets a group of CPU registers using a dictionary.
    """

    for name in dictionary:
        cpu.set_by_identifier(name, dictionary[name])


def massset(cpu, **kwargs):
    """
        Sets multiple CPU values at once.
    """
    set_by_dict(cpu, kwargs)


def assert_by_dict(cpu, kwargs):
    """
        Tests against a group of CPU registers using a dictionary.
    """

    #assert that each CPU register has the correct value
    for name in kwargs:

        #get the actual and ideal values
        actual = cpu.get_by_identifier(name)
        ideal = kwargs[name]

        #perform the actual assertion
        assert actual == ideal, repr(name) + ' was set inappropriately- was ' + repr(actual) + ', should have been ' + repr(ideal) + '.'



def massassert(cpu, **kwargs):
    """
        Performs multiple assertions on the given CPU.

        massasert(cpu, A=12, H=12)

        is the same as

        assert cpu.A==12, "A was set inappropriately"
        assert cpu.H==12, "H was set inappropriately"
    """
    assert_by_dict(cpu, kwargs)


def load_tests(loader, standard_tests, test_discovery):
    """
        Called by the test loader to automatically load all of the unit tests contained herein.
    """

    #create a new test suite
    suite = unittest.TestSuite()

    #for each test function define in the module
    for _, test in inspect.getmembers(sys.modules[__name__], inspect.isfunction):

        #if the funciton is a not a test function, continue
        if not test.__name__.startswith('test_'):
            continue

        #convert the function into a unit test, and add it to the test suite
        suite.addTest(unittest.FunctionTestCase(test))

    #return the newly created test suite
    return suite


#The following launches all of the contained tests.
#It should remain the last entry in the module.

#if this script was launched directly
if __name__ == '__main__':
    unittest.main()
