! ------------------ !
! YAO_reduced.chmech !
! ------------------ !
module fcmech
  use precision
  use string
  implicit none

  ! Chemistry related variables and parameters
  real(WP), parameter :: R_cst = 8.3144_WP

  ! Pressure
  real(WP) :: pressure

  ! Various sizes
  integer, parameter :: npS = 47
  integer, parameter :: npR = 198
  integer, parameter :: npRN = 173 ! Index of last normal reaction
  integer, parameter :: npRL = 177 ! Index of last Lindemann reaction
  integer, parameter :: nPDR = 21
  integer, parameter :: npTB= 14
  integer, parameter :: npS1 = npS+1
  integer, parameter :: npS2 = npS+2

  integer, parameter :: nreactants_max = 3
  integer, parameter :: nproducts_max = 4

  ! Properties of the species 
  real(WP), dimension(npS) :: Cpsp,hsp

  real(WP), dimension(npS,npS+1:npS+npTB) :: Meff

  ! Gas phase species
  integer, parameter :: gN2 = 1
  integer, parameter :: gO = 2
  integer, parameter :: gH2 = 3
  integer, parameter :: gH = 4
  integer, parameter :: gOH = 5
  integer, parameter :: gH2O = 6
  integer, parameter :: gH2O2 = 7
  integer, parameter :: gO2 = 8
  integer, parameter :: gHO2 = 9
  integer, parameter :: gCH2O = 10
  integer, parameter :: gCO2 = 11
  integer, parameter :: gCH3 = 12
  integer, parameter :: gCO = 13
  integer, parameter :: gC2H6 = 14
  integer, parameter :: gCH4 = 15
  integer, parameter :: gC2H4 = 16
  integer, parameter :: gC2H2 = 17
  integer, parameter :: gC3H6 = 18
  integer, parameter :: gC4H81 = 19
  integer, parameter :: gC5H10 = 20
  integer, parameter :: gC6H12 = 21
  integer, parameter :: gC7H14 = 22
  integer, parameter :: gC8H16 = 23
  integer, parameter :: gC9H18 = 24
  integer, parameter :: gC10H20 = 25
  integer, parameter :: gC12H25O2 = 26
  integer, parameter :: gNXC12H26 = 27
  integer, parameter :: gOC12H23OOH = 28
  integer, parameter :: gCH2 = 29
  integer, parameter :: gHCO = 30
  integer, parameter :: gCH2D = 31
  integer, parameter :: gCH3O = 32
  integer, parameter :: gC2H3 = 33
  integer, parameter :: gCH2CHO = 34
  integer, parameter :: gC2H5 = 35
  integer, parameter :: gAXC3H5 = 36
  integer, parameter :: gC2H3CHO = 37
  integer, parameter :: gNXC3H7 = 38
  integer, parameter :: gC4H7 = 39
  integer, parameter :: gPXC4H9 = 40
  integer, parameter :: gPXC5H11 = 41
  integer, parameter :: gPXC7H15 = 42
  integer, parameter :: gPXC12H25 = 43
  integer, parameter :: gS3XC12H25 = 44
  integer, parameter :: gSXC12H25 = 45
  integer, parameter :: gC12OOH = 46
  integer, parameter :: gO2C12H24OOH = 47

  ! Index of third bodies
  integer, parameter :: mM4 = 48
  integer, parameter :: mM5 = 49
  integer, parameter :: mM8 = 50
  integer, parameter :: mM6 = 51
  integer, parameter :: mM10 = 52
  integer, parameter :: mM12 = 53
  integer, parameter :: mM7 = 54
  integer, parameter :: mM1 = 55
  integer, parameter :: mM2 = 56
  integer, parameter :: mM13 = 57
  integer, parameter :: mM14 = 58
  integer, parameter :: mM3 = 59
  integer, parameter :: mM11 = 60
  integer, parameter :: mM9 = 61

  ! Index of reactions
  integer, parameter :: r1f = 1
  integer, parameter :: r2f = 2
  integer, parameter :: r3f = 3
  integer, parameter :: r7f = 4
  integer, parameter :: r8 = 5
  integer, parameter :: r9 = 6
  integer, parameter :: r10 = 7
  integer, parameter :: r11 = 8
  integer, parameter :: r12f = 9
  integer, parameter :: r13 = 10
  integer, parameter :: r14 = 11
  integer, parameter :: r15f = 12
  integer, parameter :: r16 = 13
  integer, parameter :: r17 = 14
  integer, parameter :: r18 = 15
  integer, parameter :: r19 = 16
  integer, parameter :: r20f = 17
  integer, parameter :: r21 = 18
  integer, parameter :: r22 = 19
  integer, parameter :: r23f = 20
  integer, parameter :: r25 = 21
  integer, parameter :: r26f = 22
  integer, parameter :: r27 = 23
  integer, parameter :: r28 = 24
  integer, parameter :: r29f = 25
  integer, parameter :: r30 = 26
  integer, parameter :: r31 = 27
  integer, parameter :: r32 = 28
  integer, parameter :: r34f = 29
  integer, parameter :: r35f = 30
  integer, parameter :: r36 = 31
  integer, parameter :: r37f = 32
  integer, parameter :: r38 = 33
  integer, parameter :: r39f = 34
  integer, parameter :: r40 = 35
  integer, parameter :: r41 = 36
  integer, parameter :: r42 = 37
  integer, parameter :: r43f = 38
  integer, parameter :: r44 = 39
  integer, parameter :: r45 = 40
  integer, parameter :: r47 = 41
  integer, parameter :: r48 = 42
  integer, parameter :: r49 = 43
  integer, parameter :: r50 = 44
  integer, parameter :: r51 = 45
  integer, parameter :: r52 = 46
  integer, parameter :: r53 = 47
  integer, parameter :: r54 = 48
  integer, parameter :: r56 = 49
  integer, parameter :: r57f = 50
  integer, parameter :: r58 = 51
  integer, parameter :: r59 = 52
  integer, parameter :: r60 = 53
  integer, parameter :: r61 = 54
  integer, parameter :: r62 = 55
  integer, parameter :: r63 = 56
  integer, parameter :: r65 = 57
  integer, parameter :: r67 = 58
  integer, parameter :: r68 = 59
  integer, parameter :: r69 = 60
  integer, parameter :: r71 = 61
  integer, parameter :: r72 = 62
  integer, parameter :: r73 = 63
  integer, parameter :: r74 = 64
  integer, parameter :: r75 = 65
  integer, parameter :: r76 = 66
  integer, parameter :: r77 = 67
  integer, parameter :: r78 = 68
  integer, parameter :: r79 = 69
  integer, parameter :: r80 = 70
  integer, parameter :: r83 = 71
  integer, parameter :: r84 = 72
  integer, parameter :: r85 = 73
  integer, parameter :: r86 = 74
  integer, parameter :: r87 = 75
  integer, parameter :: r88 = 76
  integer, parameter :: r89 = 77
  integer, parameter :: r90 = 78
  integer, parameter :: r91 = 79
  integer, parameter :: r92f = 80
  integer, parameter :: r93 = 81
  integer, parameter :: r94 = 82
  integer, parameter :: r95 = 83
  integer, parameter :: r96 = 84
  integer, parameter :: r97 = 85
  integer, parameter :: r98 = 86
  integer, parameter :: r99 = 87
  integer, parameter :: r100 = 88
  integer, parameter :: r101 = 89
  integer, parameter :: r102 = 90
  integer, parameter :: r103 = 91
  integer, parameter :: r104 = 92
  integer, parameter :: r105 = 93
  integer, parameter :: r106 = 94
  integer, parameter :: r107 = 95
  integer, parameter :: r109 = 96
  integer, parameter :: r110 = 97
  integer, parameter :: r111 = 98
  integer, parameter :: r112 = 99
  integer, parameter :: r113 = 100
  integer, parameter :: r114 = 101
  integer, parameter :: r115 = 102
  integer, parameter :: r116 = 103
  integer, parameter :: r117 = 104
  integer, parameter :: r118 = 105
  integer, parameter :: r119 = 106
  integer, parameter :: r121 = 107
  integer, parameter :: r122 = 108
  integer, parameter :: r123 = 109
  integer, parameter :: r124 = 110
  integer, parameter :: r125 = 111
  integer, parameter :: r126 = 112
  integer, parameter :: r127 = 113
  integer, parameter :: r128 = 114
  integer, parameter :: r129 = 115
  integer, parameter :: r130 = 116
  integer, parameter :: r131 = 117
  integer, parameter :: r132 = 118
  integer, parameter :: r133 = 119
  integer, parameter :: r134 = 120
  integer, parameter :: r135 = 121
  integer, parameter :: r136 = 122
  integer, parameter :: r137 = 123
  integer, parameter :: r138 = 124
  integer, parameter :: r139 = 125
  integer, parameter :: r140 = 126
  integer, parameter :: r141 = 127
  integer, parameter :: r142 = 128
  integer, parameter :: r143 = 129
  integer, parameter :: r144 = 130
  integer, parameter :: r145 = 131
  integer, parameter :: r146 = 132
  integer, parameter :: r147 = 133
  integer, parameter :: r148 = 134
  integer, parameter :: r149 = 135
  integer, parameter :: r150 = 136
  integer, parameter :: r151 = 137
  integer, parameter :: r152 = 138
  integer, parameter :: r153 = 139
  integer, parameter :: r154 = 140
  integer, parameter :: r155 = 141
  integer, parameter :: r156 = 142
  integer, parameter :: r157 = 143
  integer, parameter :: r158 = 144
  integer, parameter :: r159 = 145
  integer, parameter :: r160 = 146
  integer, parameter :: r161 = 147
  integer, parameter :: r162 = 148
  integer, parameter :: r163 = 149
  integer, parameter :: r164 = 150
  integer, parameter :: r165 = 151
  integer, parameter :: r166 = 152
  integer, parameter :: r167 = 153
  integer, parameter :: r168 = 154
  integer, parameter :: r169f = 155
  integer, parameter :: r1b = 156
  integer, parameter :: r2b = 157
  integer, parameter :: r3b = 158
  integer, parameter :: r7b = 159
  integer, parameter :: r12b = 160
  integer, parameter :: r15b = 161
  integer, parameter :: r20b = 162
  integer, parameter :: r23b = 163
  integer, parameter :: r26b = 164
  integer, parameter :: r29b = 165
  integer, parameter :: r34b = 166
  integer, parameter :: r35b = 167
  integer, parameter :: r37b = 168
  integer, parameter :: r39b = 169
  integer, parameter :: r43b = 170
  integer, parameter :: r57b = 171
  integer, parameter :: r92b = 172
  integer, parameter :: r169b = 173
  integer, parameter :: r4f = 174
  integer, parameter :: r46f = 175
  integer, parameter :: r4b = 176
  integer, parameter :: r46b = 177
  integer, parameter :: r5f = 178
  integer, parameter :: r6f = 179
  integer, parameter :: r24f = 180
  integer, parameter :: r33f = 181
  integer, parameter :: r55 = 182
  integer, parameter :: r64f = 183
  integer, parameter :: r66f = 184
  integer, parameter :: r70f = 185
  integer, parameter :: r81f = 186
  integer, parameter :: r82f = 187
  integer, parameter :: r108 = 188
  integer, parameter :: r120 = 189
  integer, parameter :: r5b = 190
  integer, parameter :: r6b = 191
  integer, parameter :: r24b = 192
  integer, parameter :: r33b = 193
  integer, parameter :: r64b = 194
  integer, parameter :: r66b = 195
  integer, parameter :: r70b = 196
  integer, parameter :: r81b = 197
  integer, parameter :: r82b = 198

  ! Molar mass  
  real(WP), parameter, dimension(npS) :: Wsp =(/ &
       0.02802_WP , & ! gN2
       0.016_WP , & ! gO
       0.002016_WP , & ! gH2
       0.001008_WP , & ! gH
       0.017008_WP , & ! gOH
       0.018016_WP , & ! gH2O
       0.034016_WP , & ! gH2O2
       0.032_WP , & ! gO2
       0.033008_WP , & ! gHO2
       0.030026_WP , & ! gCH2O
       0.04401_WP , & ! gCO2
       0.015034_WP , & ! gCH3
       0.02801_WP , & ! gCO
       0.030068_WP , & ! gC2H6
       0.016042_WP , & ! gCH4
       0.028052_WP , & ! gC2H4
       0.026036_WP , & ! gC2H2
       0.042078_WP , & ! gC3H6
       0.056104_WP , & ! gC4H81
       0.07013_WP , & ! gC5H10
       0.084156_WP , & ! gC6H12
       0.098182_WP , & ! gC7H14
       0.112208_WP , & ! gC8H16
       0.126234_WP , & ! gC9H18
       0.14026_WP , & ! gC10H20
       0.20132_WP , & ! gC12H25O2
       0.170328_WP , & ! gNXC12H26
       0.216312_WP , & ! gOC12H23OOH
       0.014026_WP , & ! gCH2
       0.029018_WP , & ! gHCO
       0.014026_WP , & ! gCH2D
       0.031034_WP , & ! gCH3O
       0.027044_WP , & ! gC2H3
       0.043044_WP , & ! gCH2CHO
       0.02906_WP , & ! gC2H5
       0.04107_WP , & ! gAXC3H5
       0.056062_WP , & ! gC2H3CHO
       0.043086_WP , & ! gNXC3H7
       0.055096_WP , & ! gC4H7
       0.057112_WP , & ! gPXC4H9
       0.071138_WP , & ! gPXC5H11
       0.09919_WP , & ! gPXC7H15
       0.16932_WP , & ! gPXC12H25
       0.16932_WP , & ! gS3XC12H25
       0.16932_WP , & ! gSXC12H25
       0.20132_WP , & ! gC12OOH
       0.23332_WP & ! gO2C12H24OOH  
  /)

  ! Arrhenius parameters - Preexponential coeff 
  real(WP), parameter, dimension(npR+nPDR) :: Acoeff =(/ &
       4.58900000e-02_WP , & ! Acoeff( r1f ) : # O+H2<=>H+OH # 
       3.97300000e-02_WP , & ! Acoeff( r2f ) : # 2OH<=>O+H2O # 
       1.02400000e+02_WP , & ! Acoeff( r3f ) : # OH+H2<=>H2O+H # 
       9.75600000e+07_WP , & ! Acoeff( r7f ) : # H+O2<=>OH+O # 
       4.00000000e+07_WP , & ! Acoeff( r8 ) : # HO2+O=>OH+O2 # 
       3.65800000e+08_WP , & ! Acoeff( r9 ) : # 2HO2=>O2+H2O2 # 
       7.48500000e+07_WP , & ! Acoeff( r10 ) : # HO2+H=>2OH # 
       1.30000000e+05_WP , & ! Acoeff( r11 ) : # 2HO2=>O2+H2O2 # 
       2.89100000e+07_WP , & ! Acoeff( r12f ) : # HO2+OH<=>H2O+O2 # 
       3.64600000e+00_WP , & ! Acoeff( r13 ) : # H+HO2=>O2+H2 # 
       2.00000000e+06_WP , & ! Acoeff( r14 ) : # H2O2+OH=>HO2+H2O # 
       2.67000000e+35_WP , & ! Acoeff( r15f ) : # H2O2+OH<=>HO2+H2O # 
       8.00000000e+07_WP , & ! Acoeff( r16 ) : # CH2+O=>HCO+H # 
       1.06000000e+07_WP , & ! Acoeff( r17 ) : # CH2+O2=>HCO+OH # 
       2.00000000e+07_WP , & ! Acoeff( r18 ) : # CH2+OH=>CH2O+H # 
       2.64000000e+06_WP , & ! Acoeff( r19 ) : # CH2+O2=>CO2+2H # 
       7.00000000e+07_WP , & ! Acoeff( r20f ) : # CH2D+H2<=>CH3+H # 
       2.80000000e+07_WP , & ! Acoeff( r21 ) : # CH2D+O2=>H+OH+CO # 
       1.20000000e+07_WP , & ! Acoeff( r22 ) : # CH2D+O2=>CO+H2O # 
       3.00000000e+07_WP , & ! Acoeff( r23f ) : # CH2D+H2O<=>CH2+H2O # 
       1.00000000e+06_WP , & ! Acoeff( r25 ) : # CH3+HO2=>CH4+O2 # 
       5.60000000e+01_WP , & ! Acoeff( r26f ) : # CH3+OH<=>CH2+H2O # 
       4.00000000e+07_WP , & ! Acoeff( r27 ) : # CH3+CH2=>C2H4+H # 
       1.34000000e+07_WP , & ! Acoeff( r28 ) : # CH3+HO2=>CH3O+OH # 
       2.50100000e+07_WP , & ! Acoeff( r29f ) : # CH3+OH<=>CH2D+H2O # 
       3.08300000e+07_WP , & ! Acoeff( r30 ) : # CH3+O2=>O+CH3O # 
       3.60000000e+04_WP , & ! Acoeff( r31 ) : # CH3+O2=>OH+CH2O # 
       8.43000000e+07_WP , & ! Acoeff( r32 ) : # CH3+O=>CH2O+H # 
       1.00000000e+02_WP , & ! Acoeff( r34f ) : # CH4+OH<=>CH3+H2O # 
       6.60000000e+02_WP , & ! Acoeff( r35f ) : # CH4+H<=>CH3+H2 # 
       1.02000000e+03_WP , & ! Acoeff( r36 ) : # CH4+O=>CH3+OH # 
       7.04600000e-02_WP , & ! Acoeff( r37f ) : # CO+OH<=>CO2+H # 
       1.57000000e-01_WP , & ! Acoeff( r38 ) : # CO+HO2=>CO2+OH # 
       5.75700000e+06_WP , & ! Acoeff( r39f ) : # CO+OH<=>CO2+H # 
       6.56200000e+06_WP , & ! Acoeff( r40 ) : # CO+CH2=>CO+CH2D # 
       3.02000000e+07_WP , & ! Acoeff( r41 ) : # HCO+OH=>CO+H2O # 
       3.00000000e+07_WP , & ! Acoeff( r42 ) : # HCO+O=>CO2+H # 
       2.24400000e+12_WP , & ! Acoeff( r43f ) : # HCO+H2O<=>CO+H+H2O # 
       1.20000000e+08_WP , & ! Acoeff( r44 ) : # HCO+H=>CO+H2 # 
       3.00000000e+07_WP , & ! Acoeff( r45 ) : # HCO+O=>CO+OH # 
       1.20400000e+04_WP , & ! Acoeff( r47 ) : # HCO+O2=>CO+HO2 # 
       1.00000000e+06_WP , & ! Acoeff( r48 ) : # CH2O+HO2=>HCO+H2O2 # 
       2.30000000e+04_WP , & ! Acoeff( r49 ) : # CH2O+H=>HCO+H2 # 
       1.00000000e+08_WP , & ! Acoeff( r50 ) : # CH2O+O2=>HCO+HO2 # 
       3.43000000e+03_WP , & ! Acoeff( r51 ) : # CH2O+OH=>HCO+H2O # 
       3.32000000e-03_WP , & ! Acoeff( r52 ) : # CH3+CH2O=>CH4+HCO # 
       3.90000000e+07_WP , & ! Acoeff( r53 ) : # CH2O+O=>HCO+OH # 
       4.28000000e-19_WP , & ! Acoeff( r54 ) : # CH3O+O2=>CH2O+HO2 # 
       1.40000000e+07_WP , & ! Acoeff( r56 ) : # CH2D+CO2=>CH2O+CO # 
       1.00000000e+01_WP , & ! Acoeff( r57f ) : # C2H2+HCO<=>C2H3+CO # 
       4.08000000e+00_WP , & ! Acoeff( r58 ) : # C2H2+O=>CH2+CO # 
       9.00000000e+07_WP , & ! Acoeff( r59 ) : # C2H3+H=>C2H2+H2 # 
       3.00000000e+05_WP , & ! Acoeff( r60 ) : # C2H3+O2=>CH2CHO+O # 
       4.60000000e+10_WP , & ! Acoeff( r61 ) : # C2H3+O2=>HCO+CH2O # 
       1.34000000e+00_WP , & ! Acoeff( r62 ) : # C2H3+O2=>C2H2+HO2 # 
       3.01100000e+07_WP , & ! Acoeff( r63 ) : # C2H3+OH=>C2H2+H2O # 
       4.80000000e+07_WP , & ! Acoeff( r65 ) : # C2H3+O=>CH3+CO # 
       3.60000000e+00_WP , & ! Acoeff( r67 ) : # C2H4+OH=>C2H3+H2O # 
       5.07000000e+01_WP , & ! Acoeff( r68 ) : # C2H4+H=>C2H3+H2 # 
       1.51000000e+01_WP , & ! Acoeff( r69 ) : # C2H4+O=>C2H3+OH # 
       1.00000000e+01_WP , & ! Acoeff( r71 ) : # C2H4+HCO=>C2H5+CO # 
       1.92000000e+01_WP , & ! Acoeff( r72 ) : # C2H4+O=>CH3+HCO # 
       2.40000000e+07_WP , & ! Acoeff( r73 ) : # C2H5+HO2=>CH3+CH2O+OH # 
       7.63100000e+11_WP , & ! Acoeff( r74 ) : # C2H5+H=>2CH3 # 
       2.00000000e+04_WP , & ! Acoeff( r75 ) : # C2H5+O2=>C2H4+HO2 # 
       3.54000000e+00_WP , & ! Acoeff( r76 ) : # C2H6+OH=>C2H5+H2O # 
       1.15000000e+02_WP , & ! Acoeff( r77 ) : # C2H6+H=>C2H5+H2 # 
       9.00000000e+07_WP , & ! Acoeff( r78 ) : # CH2CHO+H=>CH3+HCO # 
       1.80000000e+04_WP , & ! Acoeff( r79 ) : # CH2CHO+O2=>CH2O+CO+OH # 
       7.80000000e+41_WP , & ! Acoeff( r80 ) : # CH2CHO=>CH3+CO # 
       6.60000000e+06_WP , & ! Acoeff( r83 ) : # A-C3H5+HO2=>OH+C2H3+CH2O # 
       2.66000000e+06_WP , & ! Acoeff( r84 ) : # A-C3H5+HO2=>C3H6+O2 # 
       5.67600000e+26_WP , & ! Acoeff( r85 ) : # H+A-C3H5=>CH3+C2H3 # 
       6.00000000e+07_WP , & ! Acoeff( r86 ) : # A-C3H5+O=>C2H3CHO+H # 
       8.00000000e+15_WP , & ! Acoeff( r87 ) : # C3H6+H=>C2H4+CH3 # 
       1.73000000e-01_WP , & ! Acoeff( r88 ) : # C3H6+H=>A-C3H5+H2 # 
       3.50000000e+01_WP , & ! Acoeff( r89 ) : # C3H6+O=>C2H5+HCO # 
       4.00000000e+01_WP , & ! Acoeff( r90 ) : # C3H6+O=>C2H3CHO+2H # 
       3.10000000e+00_WP , & ! Acoeff( r91 ) : # C3H6+OH=>A-C3H5+H2O # 
       9.60000000e+13_WP , & ! Acoeff( r92f ) : # N-C3H7<=>CH3+C2H4 # 
       9.00000000e+04_WP , & ! Acoeff( r93 ) : # N-C3H7+O2=>C3H6+HO2 # 
       2.40000000e+07_WP , & ! Acoeff( r94 ) : # N-C3H7+HO2=>C2H5+OH+CH2O # 
       3.43000000e+03_WP , & ! Acoeff( r95 ) : # C2H3CHO+OH=>C2H3+H2O+CO # 
       2.40000000e+07_WP , & ! Acoeff( r96 ) : # C4H7+HO2=>CH2O+OH+A-C3H5 # 
       1.20500000e+47_WP , & ! Acoeff( r97 ) : # C4H7=>C2H3+C2H4 # 
       2.60000000e+07_WP , & ! Acoeff( r98 ) : # C4H81+O=>C4H7+OH # 
       1.60000000e+16_WP , & ! Acoeff( r99 ) : # C4H81+H=>C2H4+C2H5 # 
       3.30000000e+02_WP , & ! Acoeff( r100 ) : # C4H81+O=>N-C3H7+HCO # 
       6.50000000e-01_WP , & ! Acoeff( r101 ) : # C4H81+H=>C4H7+H2 # 
       3.20000000e+16_WP , & ! Acoeff( r102 ) : # C4H81+H=>C3H6+CH3 # 
       1.10000000e+07_WP , & ! Acoeff( r103 ) : # P-C4H9+CH3=>C4H81+CH4 # 
       9.60000000e+07_WP , & ! Acoeff( r104 ) : # P-C4H9+O=>N-C3H7+CH2O # 
       3.70000000e+18_WP , & ! Acoeff( r105 ) : # P-C4H9+H=>2C2H5 # 
       2.40000000e+07_WP , & ! Acoeff( r106 ) : # P-C4H9+HO2=>N-C3H7+OH+CH2O # 
       2.40000000e+07_WP , & ! Acoeff( r107 ) : # P-C4H9+OH=>C4H81+H2O # 
       2.70000000e+05_WP , & ! Acoeff( r109 ) : # P-C4H9+O2=>C4H81+HO2 # 
       1.60000000e+16_WP , & ! Acoeff( r110 ) : # C5H10+H=>C3H6+C2H5 # 
       8.00000000e+15_WP , & ! Acoeff( r111 ) : # C5H10+H=>C2H4+N-C3H7 # 
       1.79200000e+20_WP , & ! Acoeff( r112 ) : # P-C5H11=>N-C3H7+C2H4 # 
       1.60000000e+16_WP , & ! Acoeff( r113 ) : # C6H12+H=>C3H6+N-C3H7 # 
       8.00000000e+15_WP , & ! Acoeff( r114 ) : # C6H12+H=>C2H4+P-C4H9 # 
       1.60000000e+16_WP , & ! Acoeff( r115 ) : # C7H14+H=>C3H6+P-C4H9 # 
       8.00000000e+15_WP , & ! Acoeff( r116 ) : # C7H14+H=>C2H4+P-C5H11 # 
       1.28400000e+20_WP , & ! Acoeff( r117 ) : # P-C7H15=>P-C5H11+C2H4 # 
       8.00000000e+15_WP , & ! Acoeff( r118 ) : # C8H16+H=>2C2H4+P-C4H9 # 
       1.60000000e+16_WP , & ! Acoeff( r119 ) : # C8H16+H=>C3H6+P-C5H11 # 
       1.60000000e+16_WP , & ! Acoeff( r121 ) : # C9H18+H=>C3H6+P-C4H9+C2H4 # 
       8.00000000e+15_WP , & ! Acoeff( r122 ) : # C9H18+H=>C2H4+P-C7H15 # 
       1.60000000e+16_WP , & ! Acoeff( r123 ) : # C10H20+H=>C3H6+P-C7H15 # 
       8.00000000e+15_WP , & ! Acoeff( r124 ) : # C10H20+H=>3C2H4+P-C4H9 # 
       3.67000000e+12_WP , & ! Acoeff( r125 ) : # P-C12H25=>S3-C12H25 # 
       1.31200000e+20_WP , & ! Acoeff( r126 ) : # P-C12H25=>P-C4H9+4C2H4 # 
       5.00000000e+07_WP , & ! Acoeff( r127 ) : # P-C12H25+O2=>C12H25O2 # 
       5.00000000e+07_WP , & ! Acoeff( r128 ) : # S3-C12H25+O2=>C12H25O2 # 
       4.61900000e+19_WP , & ! Acoeff( r129 ) : # S3-C12H25=>P-C4H9+C2H4+C6H12 # 
       3.84800000e+19_WP , & ! Acoeff( r130 ) : # S3-C12H25=>C2H5+C10H20 # 
       6.23300000e+19_WP , & ! Acoeff( r131 ) : # S3-C12H25=>N-C3H7+C9H18 # 
       4.03600000e+19_WP , & ! Acoeff( r132 ) : # S3-C12H25=>P-C4H9+C8H16 # 
       4.63900000e+19_WP , & ! Acoeff( r133 ) : # S3-C12H25=>P-C7H15+C5H10 # 
       4.45800000e+19_WP , & ! Acoeff( r134 ) : # S3-C12H25=>P-C5H11+C7H14 # 
       1.68200000e+20_WP , & ! Acoeff( r135 ) : # S-C12H25=>P-C7H15+C2H4+C3H6 # 
       5.00000000e+07_WP , & ! Acoeff( r136 ) : # S-C12H25+O2=>C12H25O2 # 
       4.72100000e+19_WP , & ! Acoeff( r137 ) : # S-C12H25=>P-C4H9+2C2H4+C4H81 # 
       1.65400000e+26_WP , & ! Acoeff( r138 ) : # N-C12H26=>P-C5H11+P-C7H15 # 
       6.76000000e-02_WP , & ! Acoeff( r139 ) : # N-C12H26+HO2=>P-C12H25+H2O2 # 
       1.35600000e+27_WP , & ! Acoeff( r140 ) : # N-C12H26=>N-C3H7+P-C7H15+C2H4 # 
       1.81000000e-06_WP , & ! Acoeff( r141 ) : # N-C12H26+CH3=>P-C12H25+CH4 # 
       9.52000000e-02_WP , & ! Acoeff( r142 ) : # N-C12H26+O=>S-C12H25+OH # 
       8.85000000e-02_WP , & ! Acoeff( r143 ) : # N-C12H26+HO2=>S3-C12H25+H2O2 # 
       1.30000000e+00_WP , & ! Acoeff( r144 ) : # N-C12H26+H=>P-C12H25+H2 # 
       1.39600000e+26_WP , & ! Acoeff( r145 ) : # N-C12H26=>C2H5+P-C4H9+3C2H4 # 
       1.90000000e-01_WP , & ! Acoeff( r146 ) : # N-C12H26+O=>P-C12H25+OH # 
       1.46300000e+26_WP , & ! Acoeff( r147 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       6.00000000e-06_WP , & ! Acoeff( r148 ) : # N-C12H26+CH3=>S-C12H25+CH4 # 
       1.01000000e-01_WP , & ! Acoeff( r149 ) : # N-C12H26+OH=>S3-C12H25+H2O # 
       1.20000000e+08_WP , & ! Acoeff( r150 ) : # N-C12H26+O2=>S3-C12H25+HO2 # 
       1.67800000e+26_WP , & ! Acoeff( r151 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       3.40000000e-03_WP , & ! Acoeff( r152 ) : # N-C12H26+OH=>P-C12H25+H2O # 
       2.60000000e+00_WP , & ! Acoeff( r153 ) : # N-C12H26+H=>S-C12H25+H2 # 
       3.90000000e+00_WP , & ! Acoeff( r154 ) : # N-C12H26+H=>S3-C12H25+H2 # 
       8.90000000e-02_WP , & ! Acoeff( r155 ) : # N-C12H26+HO2=>S-C12H25+H2O2 # 
       9.00000000e-06_WP , & ! Acoeff( r156 ) : # N-C12H26+CH3=>S3-C12H25+CH4 # 
       8.00000000e+07_WP , & ! Acoeff( r157 ) : # N-C12H26+O2=>S-C12H25+HO2 # 
       1.42800000e-01_WP , & ! Acoeff( r158 ) : # N-C12H26+O=>S3-C12H25+OH # 
       7.40000000e-02_WP , & ! Acoeff( r159 ) : # N-C12H26+OH=>S-C12H25+H2O # 
       2.75000000e+13_WP , & ! Acoeff( r160 ) : # C12H25O2=>S3-C12H25+O2 # 
       2.75000000e+13_WP , & ! Acoeff( r161 ) : # C12H25O2=>S-C12H25+O2 # 
       2.75000000e+13_WP , & ! Acoeff( r162 ) : # C12H25O2=>P-C12H25+O2 # 
       1.51000000e+12_WP , & ! Acoeff( r163 ) : # C12H25O2=>C12OOH # 
       1.00000000e+11_WP , & ! Acoeff( r164 ) : # C12OOH=>C12H25O2 # 
       4.60000000e+04_WP , & ! Acoeff( r165 ) : # C12OOH+O2=>O2C12H24OOH # 
       1.80000000e+15_WP , & ! Acoeff( r166 ) : # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH # 
       8.90000000e+10_WP , & ! Acoeff( r167 ) : # O2C12H24OOH=>OC12H23OOH+OH # 
       2.51000000e+13_WP , & ! Acoeff( r168 ) : # O2C12H24OOH=>C12OOH+O2 # 
       1.50000000e+07_WP , & ! Acoeff( r169f ) : # CH2D+N2<=>CH2+N2 # 
       2.75113550e-02_WP , & ! Acoeff( r1b ) : # REV - O+H2<=>H+OH # 
       7.29083537e-01_WP , & ! Acoeff( r2b ) : # REV - 2OH<=>O+H2O # 
       1.12655554e+03_WP , & ! Acoeff( r3b ) : # REV - OH+H2<=>H2O+H # 
       1.94996974e+05_WP , & ! Acoeff( r7b ) : # REV - H+O2<=>OH+O # 
       5.16064694e+07_WP , & ! Acoeff( r12b ) : # REV - HO2+OH<=>H2O+O2 # 
       9.96133968e+33_WP , & ! Acoeff( r15b ) : # REV - H2O2+OH<=>HO2+H2O # 
       2.25605749e+10_WP , & ! Acoeff( r20b ) : # REV - CH2D+H2<=>CH3+H # 
       2.18728179e+07_WP , & ! Acoeff( r23b ) : # REV - CH2D+H2O<=>CH2+H2O # 
       1.39370878e+00_WP , & ! Acoeff( r26b ) : # REV - CH3+OH<=>CH2+H2O # 
       8.53717569e+05_WP , & ! Acoeff( r29b ) : # REV - CH3+OH<=>CH2D+H2O # 
       6.31494891e-01_WP , & ! Acoeff( r34b ) : # REV - CH4+OH<=>CH3+H2O # 
       3.78844623e-01_WP , & ! Acoeff( r35b ) : # REV - CH4+H<=>CH3+H2 # 
       3.85757198e+05_WP , & ! Acoeff( r37b ) : # REV - CO+OH<=>CO2+H # 
       3.15186515e+13_WP , & ! Acoeff( r39b ) : # REV - CO+OH<=>CO2+H # 
       4.23678237e+05_WP , & ! Acoeff( r43b ) : # REV - HCO+H2O<=>CO+H+H2O # 
       4.18364781e-01_WP , & ! Acoeff( r57b ) : # REV - C2H2+HCO<=>C2H3+CO # 
       1.39094803e+01_WP , & ! Acoeff( r92b ) : # REV - N-C3H7<=>CH3+C2H4 # 
       1.09364090e+07_WP , & ! Acoeff( r169b ) : # REV - CH2D+N2<=>CH2+N2 # 
       4.40000000e+10_WP , & ! Acoeff( r4f ) : # H+OH+M<=>H2O+M # 
       1.87000000e+11_WP , & ! Acoeff( r46f ) : # HCO+M<=>CO+H+M # 
       1.13142261e+17_WP , & ! Acoeff( r4b ) : # REV - H+OH+M<=>H2O+M # 
       3.53065198e+04_WP , & ! Acoeff( r46b ) : # REV - HCO+M<=>CO+H+M # 
       2.01000000e+05_WP , & ! Acoeff_0( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       6.32800000e+07_WP , & ! Acoeff_0( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       1.77000000e+38_WP , & ! Acoeff_0( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       2.47700000e+21_WP , & ! Acoeff_0( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       3.53200000e+27_WP , & ! Acoeff_0( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       4.27000000e+46_WP , & ! Acoeff_0( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       2.56500000e+21_WP , & ! Acoeff_0( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       4.71500000e+06_WP , & ! Acoeff_0( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       3.91000000e+48_WP , & ! Acoeff_0( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1.33000000e+48_WP , & ! Acoeff_0( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       5.61500000e+33_WP , & ! Acoeff_0( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       6.26000000e+26_WP , & ! Acoeff_0( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       7.12538504e+16_WP , & ! Acoeff_0( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       9.11554401e+13_WP , & ! Acoeff_0( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       3.36285150e+52_WP , & ! Acoeff_0( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       1.00862178e+30_WP , & ! Acoeff_0( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       4.75055293e+61_WP , & ! Acoeff_0( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       1.15756545e+16_WP , & ! Acoeff_0( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       8.80958623e+12_WP , & ! Acoeff_0( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       4.08296436e+61_WP , & ! Acoeff_0( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       3.91035855e+54_WP , & ! Acoeff_0( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
       1.11000000e+08_WP , & ! Acoeff_inf( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       5.11600000e+06_WP , & ! Acoeff_inf( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       2.12000000e+10_WP , & ! Acoeff_inf( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       1.27000000e+10_WP , & ! Acoeff_inf( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       8.66900000e+14_WP , & ! Acoeff_inf( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       2.50000000e+07_WP , & ! Acoeff_inf( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       3.86000000e+07_WP , & ! Acoeff_inf( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       3.97500000e+03_WP , & ! Acoeff_inf( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       1.00000000e+08_WP , & ! Acoeff_inf( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       2.00000000e+08_WP , & ! Acoeff_inf( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1.19300000e+14_WP , & ! Acoeff_inf( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1.33000000e+07_WP , & ! Acoeff_inf( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       3.93491413e+19_WP , & ! Acoeff_inf( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       7.36964651e+12_WP , & ! Acoeff_inf( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       4.02782213e+24_WP , & ! Acoeff_inf( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       5.17137531e+18_WP , & ! Acoeff_inf( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       2.78135417e+22_WP , & ! Acoeff_inf( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       1.74198933e+02_WP , & ! Acoeff_inf( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       7.42695764e+09_WP , & ! Acoeff_inf( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       1.04423641e+21_WP , & ! Acoeff_inf( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       5.88023842e+14_WP  & ! Acoeff_inf( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! Arrhenius parameters - Temperatire exponent 
  real(WP), parameter, dimension(npR+nPDR) :: ncoeff =(/ &
       2.700_WP , & ! ncoeff( r1f ) : # O+H2<=>H+OH # 
       2.400_WP , & ! ncoeff( r2f ) : # 2OH<=>O+H2O # 
       1.600_WP , & ! ncoeff( r3f ) : # OH+H2<=>H2O+H # 
       0.000_WP , & ! ncoeff( r7f ) : # H+O2<=>OH+O # 
       0.000_WP , & ! ncoeff( r8 ) : # HO2+O=>OH+O2 # 
       0.000_WP , & ! ncoeff( r9 ) : # 2HO2=>O2+H2O2 # 
       0.000_WP , & ! ncoeff( r10 ) : # HO2+H=>2OH # 
       0.000_WP , & ! ncoeff( r11 ) : # 2HO2=>O2+H2O2 # 
       0.000_WP , & ! ncoeff( r12f ) : # HO2+OH<=>H2O+O2 # 
       2.071_WP , & ! ncoeff( r13 ) : # H+HO2=>O2+H2 # 
       0.000_WP , & ! ncoeff( r14 ) : # H2O2+OH=>HO2+H2O # 
       -7.000_WP , & ! ncoeff( r15f ) : # H2O2+OH<=>HO2+H2O # 
       0.000_WP , & ! ncoeff( r16 ) : # CH2+O=>HCO+H # 
       0.000_WP , & ! ncoeff( r17 ) : # CH2+O2=>HCO+OH # 
       0.000_WP , & ! ncoeff( r18 ) : # CH2+OH=>CH2O+H # 
       0.000_WP , & ! ncoeff( r19 ) : # CH2+O2=>CO2+2H # 
       0.000_WP , & ! ncoeff( r20f ) : # CH2D+H2<=>CH3+H # 
       0.000_WP , & ! ncoeff( r21 ) : # CH2D+O2=>H+OH+CO # 
       0.000_WP , & ! ncoeff( r22 ) : # CH2D+O2=>CO+H2O # 
       0.000_WP , & ! ncoeff( r23f ) : # CH2D+H2O<=>CH2+H2O # 
       0.000_WP , & ! ncoeff( r25 ) : # CH3+HO2=>CH4+O2 # 
       1.600_WP , & ! ncoeff( r26f ) : # CH3+OH<=>CH2+H2O # 
       0.000_WP , & ! ncoeff( r27 ) : # CH3+CH2=>C2H4+H # 
       0.000_WP , & ! ncoeff( r28 ) : # CH3+HO2=>CH3O+OH # 
       0.000_WP , & ! ncoeff( r29f ) : # CH3+OH<=>CH2D+H2O # 
       0.000_WP , & ! ncoeff( r30 ) : # CH3+O2=>O+CH3O # 
       0.000_WP , & ! ncoeff( r31 ) : # CH3+O2=>OH+CH2O # 
       0.000_WP , & ! ncoeff( r32 ) : # CH3+O=>CH2O+H # 
       1.600_WP , & ! ncoeff( r34f ) : # CH4+OH<=>CH3+H2O # 
       1.620_WP , & ! ncoeff( r35f ) : # CH4+H<=>CH3+H2 # 
       1.500_WP , & ! ncoeff( r36 ) : # CH4+O=>CH3+OH # 
       2.053_WP , & ! ncoeff( r37f ) : # CO+OH<=>CO2+H # 
       2.180_WP , & ! ncoeff( r38 ) : # CO+HO2=>CO2+OH # 
       -0.664_WP , & ! ncoeff( r39f ) : # CO+OH<=>CO2+H # 
       -0.062_WP , & ! ncoeff( r40 ) : # CO+CH2=>CO+CH2D # 
       0.000_WP , & ! ncoeff( r41 ) : # HCO+OH=>CO+H2O # 
       0.000_WP , & ! ncoeff( r42 ) : # HCO+O=>CO2+H # 
       -1.000_WP , & ! ncoeff( r43f ) : # HCO+H2O<=>CO+H+H2O # 
       0.000_WP , & ! ncoeff( r44 ) : # HCO+H=>CO+H2 # 
       0.000_WP , & ! ncoeff( r45 ) : # HCO+O=>CO+OH # 
       0.807_WP , & ! ncoeff( r47 ) : # HCO+O2=>CO+HO2 # 
       0.000_WP , & ! ncoeff( r48 ) : # CH2O+HO2=>HCO+H2O2 # 
       1.050_WP , & ! ncoeff( r49 ) : # CH2O+H=>HCO+H2 # 
       0.000_WP , & ! ncoeff( r50 ) : # CH2O+O2=>HCO+HO2 # 
       1.180_WP , & ! ncoeff( r51 ) : # CH2O+OH=>HCO+H2O # 
       2.810_WP , & ! ncoeff( r52 ) : # CH3+CH2O=>CH4+HCO # 
       0.000_WP , & ! ncoeff( r53 ) : # CH2O+O=>HCO+OH # 
       7.600_WP , & ! ncoeff( r54 ) : # CH3O+O2=>CH2O+HO2 # 
       0.000_WP , & ! ncoeff( r56 ) : # CH2D+CO2=>CH2O+CO # 
       2.000_WP , & ! ncoeff( r57f ) : # C2H2+HCO<=>C2H3+CO # 
       2.000_WP , & ! ncoeff( r58 ) : # C2H2+O=>CH2+CO # 
       0.000_WP , & ! ncoeff( r59 ) : # C2H3+H=>C2H2+H2 # 
       0.290_WP , & ! ncoeff( r60 ) : # C2H3+O2=>CH2CHO+O # 
       -1.390_WP , & ! ncoeff( r61 ) : # C2H3+O2=>HCO+CH2O # 
       1.610_WP , & ! ncoeff( r62 ) : # C2H3+O2=>C2H2+HO2 # 
       0.000_WP , & ! ncoeff( r63 ) : # C2H3+OH=>C2H2+H2O # 
       0.000_WP , & ! ncoeff( r65 ) : # C2H3+O=>CH3+CO # 
       2.000_WP , & ! ncoeff( r67 ) : # C2H4+OH=>C2H3+H2O # 
       1.900_WP , & ! ncoeff( r68 ) : # C2H4+H=>C2H3+H2 # 
       1.900_WP , & ! ncoeff( r69 ) : # C2H4+O=>C2H3+OH # 
       2.000_WP , & ! ncoeff( r71 ) : # C2H4+HCO=>C2H5+CO # 
       1.830_WP , & ! ncoeff( r72 ) : # C2H4+O=>CH3+HCO # 
       0.000_WP , & ! ncoeff( r73 ) : # C2H5+HO2=>CH3+CH2O+OH # 
       -1.076_WP , & ! ncoeff( r74 ) : # C2H5+H=>2CH3 # 
       0.000_WP , & ! ncoeff( r75 ) : # C2H5+O2=>C2H4+HO2 # 
       2.120_WP , & ! ncoeff( r76 ) : # C2H6+OH=>C2H5+H2O # 
       1.900_WP , & ! ncoeff( r77 ) : # C2H6+H=>C2H5+H2 # 
       0.000_WP , & ! ncoeff( r78 ) : # CH2CHO+H=>CH3+HCO # 
       0.000_WP , & ! ncoeff( r79 ) : # CH2CHO+O2=>CH2O+CO+OH # 
       -9.147_WP , & ! ncoeff( r80 ) : # CH2CHO=>CH3+CO # 
       0.000_WP , & ! ncoeff( r83 ) : # A-C3H5+HO2=>OH+C2H3+CH2O # 
       0.000_WP , & ! ncoeff( r84 ) : # A-C3H5+HO2=>C3H6+O2 # 
       -4.718_WP , & ! ncoeff( r85 ) : # H+A-C3H5=>CH3+C2H3 # 
       0.000_WP , & ! ncoeff( r86 ) : # A-C3H5+O=>C2H3CHO+H # 
       -2.390_WP , & ! ncoeff( r87 ) : # C3H6+H=>C2H4+CH3 # 
       2.500_WP , & ! ncoeff( r88 ) : # C3H6+H=>A-C3H5+H2 # 
       1.650_WP , & ! ncoeff( r89 ) : # C3H6+O=>C2H5+HCO # 
       1.650_WP , & ! ncoeff( r90 ) : # C3H6+O=>C2H3CHO+2H # 
       2.000_WP , & ! ncoeff( r91 ) : # C3H6+OH=>A-C3H5+H2O # 
       0.000_WP , & ! ncoeff( r92f ) : # N-C3H7<=>CH3+C2H4 # 
       0.000_WP , & ! ncoeff( r93 ) : # N-C3H7+O2=>C3H6+HO2 # 
       0.000_WP , & ! ncoeff( r94 ) : # N-C3H7+HO2=>C2H5+OH+CH2O # 
       1.180_WP , & ! ncoeff( r95 ) : # C2H3CHO+OH=>C2H3+H2O+CO # 
       0.000_WP , & ! ncoeff( r96 ) : # C4H7+HO2=>CH2O+OH+A-C3H5 # 
       -10.519_WP , & ! ncoeff( r97 ) : # C4H7=>C2H3+C2H4 # 
       0.000_WP , & ! ncoeff( r98 ) : # C4H81+O=>C4H7+OH # 
       -2.390_WP , & ! ncoeff( r99 ) : # C4H81+H=>C2H4+C2H5 # 
       1.450_WP , & ! ncoeff( r100 ) : # C4H81+O=>N-C3H7+HCO # 
       2.540_WP , & ! ncoeff( r101 ) : # C4H81+H=>C4H7+H2 # 
       -2.390_WP , & ! ncoeff( r102 ) : # C4H81+H=>C3H6+CH3 # 
       0.000_WP , & ! ncoeff( r103 ) : # P-C4H9+CH3=>C4H81+CH4 # 
       0.000_WP , & ! ncoeff( r104 ) : # P-C4H9+O=>N-C3H7+CH2O # 
       -2.920_WP , & ! ncoeff( r105 ) : # P-C4H9+H=>2C2H5 # 
       0.000_WP , & ! ncoeff( r106 ) : # P-C4H9+HO2=>N-C3H7+OH+CH2O # 
       0.000_WP , & ! ncoeff( r107 ) : # P-C4H9+OH=>C4H81+H2O # 
       0.000_WP , & ! ncoeff( r109 ) : # P-C4H9+O2=>C4H81+HO2 # 
       -2.390_WP , & ! ncoeff( r110 ) : # C5H10+H=>C3H6+C2H5 # 
       -2.390_WP , & ! ncoeff( r111 ) : # C5H10+H=>C2H4+N-C3H7 # 
       -2.102_WP , & ! ncoeff( r112 ) : # P-C5H11=>N-C3H7+C2H4 # 
       -2.390_WP , & ! ncoeff( r113 ) : # C6H12+H=>C3H6+N-C3H7 # 
       -2.390_WP , & ! ncoeff( r114 ) : # C6H12+H=>C2H4+P-C4H9 # 
       -2.390_WP , & ! ncoeff( r115 ) : # C7H14+H=>C3H6+P-C4H9 # 
       -2.390_WP , & ! ncoeff( r116 ) : # C7H14+H=>C2H4+P-C5H11 # 
       -2.061_WP , & ! ncoeff( r117 ) : # P-C7H15=>P-C5H11+C2H4 # 
       -2.390_WP , & ! ncoeff( r118 ) : # C8H16+H=>2C2H4+P-C4H9 # 
       -2.390_WP , & ! ncoeff( r119 ) : # C8H16+H=>C3H6+P-C5H11 # 
       -2.390_WP , & ! ncoeff( r121 ) : # C9H18+H=>C3H6+P-C4H9+C2H4 # 
       -2.390_WP , & ! ncoeff( r122 ) : # C9H18+H=>C2H4+P-C7H15 # 
       -2.390_WP , & ! ncoeff( r123 ) : # C10H20+H=>C3H6+P-C7H15 # 
       -2.390_WP , & ! ncoeff( r124 ) : # C10H20+H=>3C2H4+P-C4H9 # 
       -0.600_WP , & ! ncoeff( r125 ) : # P-C12H25=>S3-C12H25 # 
       -2.064_WP , & ! ncoeff( r126 ) : # P-C12H25=>P-C4H9+4C2H4 # 
       0.000_WP , & ! ncoeff( r127 ) : # P-C12H25+O2=>C12H25O2 # 
       0.000_WP , & ! ncoeff( r128 ) : # S3-C12H25+O2=>C12H25O2 # 
       -1.805_WP , & ! ncoeff( r129 ) : # S3-C12H25=>P-C4H9+C2H4+C6H12 # 
       -1.822_WP , & ! ncoeff( r130 ) : # S3-C12H25=>C2H5+C10H20 # 
       -1.841_WP , & ! ncoeff( r131 ) : # S3-C12H25=>N-C3H7+C9H18 # 
       -1.694_WP , & ! ncoeff( r132 ) : # S3-C12H25=>P-C4H9+C8H16 # 
       -1.805_WP , & ! ncoeff( r133 ) : # S3-C12H25=>P-C7H15+C5H10 # 
       -1.801_WP , & ! ncoeff( r134 ) : # S3-C12H25=>P-C5H11+C7H14 # 
       -1.914_WP , & ! ncoeff( r135 ) : # S-C12H25=>P-C7H15+C2H4+C3H6 # 
       0.000_WP , & ! ncoeff( r136 ) : # S-C12H25+O2=>C12H25O2 # 
       -1.823_WP , & ! ncoeff( r137 ) : # S-C12H25=>P-C4H9+2C2H4+C4H81 # 
       -2.744_WP , & ! ncoeff( r138 ) : # N-C12H26=>P-C5H11+P-C7H15 # 
       2.550_WP , & ! ncoeff( r139 ) : # N-C12H26+HO2=>P-C12H25+H2O2 # 
       -2.782_WP , & ! ncoeff( r140 ) : # N-C12H26=>N-C3H7+P-C7H15+C2H4 # 
       3.650_WP , & ! ncoeff( r141 ) : # N-C12H26+CH3=>P-C12H25+CH4 # 
       2.710_WP , & ! ncoeff( r142 ) : # N-C12H26+O=>S-C12H25+OH # 
       2.600_WP , & ! ncoeff( r143 ) : # N-C12H26+HO2=>S3-C12H25+H2O2 # 
       2.540_WP , & ! ncoeff( r144 ) : # N-C12H26+H=>P-C12H25+H2 # 
       -2.763_WP , & ! ncoeff( r145 ) : # N-C12H26=>C2H5+P-C4H9+3C2H4 # 
       2.680_WP , & ! ncoeff( r146 ) : # N-C12H26+O=>P-C12H25+OH # 
       -2.635_WP , & ! ncoeff( r147 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       3.460_WP , & ! ncoeff( r148 ) : # N-C12H26+CH3=>S-C12H25+CH4 # 
       2.390_WP , & ! ncoeff( r149 ) : # N-C12H26+OH=>S3-C12H25+H2O # 
       0.000_WP , & ! ncoeff( r150 ) : # N-C12H26+O2=>S3-C12H25+HO2 # 
       -2.746_WP , & ! ncoeff( r151 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       2.660_WP , & ! ncoeff( r152 ) : # N-C12H26+OH=>P-C12H25+H2O # 
       2.400_WP , & ! ncoeff( r153 ) : # N-C12H26+H=>S-C12H25+H2 # 
       2.400_WP , & ! ncoeff( r154 ) : # N-C12H26+H=>S3-C12H25+H2 # 
       2.600_WP , & ! ncoeff( r155 ) : # N-C12H26+HO2=>S-C12H25+H2O2 # 
       3.460_WP , & ! ncoeff( r156 ) : # N-C12H26+CH3=>S3-C12H25+CH4 # 
       0.000_WP , & ! ncoeff( r157 ) : # N-C12H26+O2=>S-C12H25+HO2 # 
       2.710_WP , & ! ncoeff( r158 ) : # N-C12H26+O=>S3-C12H25+OH # 
       2.390_WP , & ! ncoeff( r159 ) : # N-C12H26+OH=>S-C12H25+H2O # 
       0.000_WP , & ! ncoeff( r160 ) : # C12H25O2=>S3-C12H25+O2 # 
       0.000_WP , & ! ncoeff( r161 ) : # C12H25O2=>S-C12H25+O2 # 
       0.000_WP , & ! ncoeff( r162 ) : # C12H25O2=>P-C12H25+O2 # 
       0.000_WP , & ! ncoeff( r163 ) : # C12H25O2=>C12OOH # 
       0.000_WP , & ! ncoeff( r164 ) : # C12OOH=>C12H25O2 # 
       0.000_WP , & ! ncoeff( r165 ) : # C12OOH+O2=>O2C12H24OOH # 
       0.000_WP , & ! ncoeff( r166 ) : # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH # 
       0.000_WP , & ! ncoeff( r167 ) : # O2C12H24OOH=>OC12H23OOH+OH # 
       0.000_WP , & ! ncoeff( r168 ) : # O2C12H24OOH=>C12OOH+O2 # 
       0.000_WP , & ! ncoeff( r169f ) : # CH2D+N2<=>CH2+N2 # 
       2.6638_WP , & ! ncoeff( r1b ) : # REV - O+H2<=>H+OH # 
       2.3324_WP , & ! ncoeff( r2b ) : # REV - 2OH<=>O+H2O # 
       1.4963_WP , & ! ncoeff( r3b ) : # REV - OH+H2<=>H2O+H # 
       0.4357_WP , & ! ncoeff( r7b ) : # REV - H+O2<=>OH+O # 
       0.2583_WP , & ! ncoeff( r12b ) : # REV - HO2+OH<=>H2O+O2 # 
       -6.4968_WP , & ! ncoeff( r15b ) : # REV - H2O2+OH<=>HO2+H2O # 
       -0.5816_WP , & ! ncoeff( r20b ) : # REV - CH2D+H2<=>CH3+H # 
       -0.0623_WP , & ! ncoeff( r23b ) : # REV - CH2D+H2O<=>CH2+H2O # 
       2.0156_WP , & ! ncoeff( r26b ) : # REV - CH3+OH<=>CH2+H2O # 
       0.4779_WP , & ! ncoeff( r29b ) : # REV - CH3+OH<=>CH2D+H2O # 
       2.0078_WP , & ! ncoeff( r34b ) : # REV - CH4+OH<=>CH3+H2O # 
       2.1316_WP , & ! ncoeff( r35b ) : # REV - CH4+H<=>CH3+H2 # 
       0.7756_WP , & ! ncoeff( r37b ) : # REV - CO+OH<=>CO2+H # 
       -1.9414_WP , & ! ncoeff( r39b ) : # REV - CO+OH<=>CO2+H # 
       -0.7673_WP , & ! ncoeff( r43b ) : # REV - HCO+H2O<=>CO+H+H2O # 
       2.4046_WP , & ! ncoeff( r57b ) : # REV - C2H2+HCO<=>C2H3+CO # 
       1.6617_WP , & ! ncoeff( r92b ) : # REV - N-C3H7<=>CH3+C2H4 # 
       -0.0623_WP , & ! ncoeff( r169b ) : # REV - CH2D+N2<=>CH2+N2 # 
       -2.000_WP , & ! ncoeff( r4f ) : # H+OH+M<=>H2O+M # 
       -1.000_WP , & ! ncoeff( r46f ) : # HCO+M<=>CO+H+M # 
       -1.7535_WP , & ! ncoeff( r4b ) : # REV - H+OH+M<=>H2O+M # 
       -0.7673_WP , & ! ncoeff( r46b ) : # REV - HCO+M<=>CO+H+M # 
       -0.584_WP , & ! ncoeff_0( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       -1.400_WP , & ! ncoeff_0( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       -9.670_WP , & ! ncoeff_0( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       -4.760_WP , & ! ncoeff_0( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       -5.675_WP , & ! ncoeff_0( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       -11.940_WP , & ! ncoeff_0( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       -3.400_WP , & ! ncoeff_0( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       0.000_WP , & ! ncoeff_0( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       -12.810_WP , & ! ncoeff_0( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       -12.000_WP , & ! ncoeff_0( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       -7.005_WP , & ! ncoeff_0( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       -6.660_WP , & ! ncoeff_0( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       -1.6024_WP , & ! ncoeff_0( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       -1.4118_WP , & ! ncoeff_0( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       -11.0554_WP , & ! ncoeff_0( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       -4.9213_WP , & ! ncoeff_0( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       -13.4483_WP , & ! ncoeff_0( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       -3.5719_WP , & ! ncoeff_0( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       -0.1213_WP , & ! ncoeff_0( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       -14.0807_WP , & ! ncoeff_0( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       -11.6203_WP , & ! ncoeff_0( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
       -0.370_WP , & ! ncoeff_inf( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.440_WP , & ! ncoeff_inf( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       -0.970_WP , & ! ncoeff_inf( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       -0.630_WP , & ! ncoeff_inf( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       -0.421_WP , & ! ncoeff_inf( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       0.000_WP , & ! ncoeff_inf( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       1.620_WP , & ! ncoeff_inf( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       1.280_WP , & ! ncoeff_inf( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       -0.320_WP , & ! ncoeff_inf( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.000_WP , & ! ncoeff_inf( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       -0.345_WP , & ! ncoeff_inf( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       0.000_WP , & ! ncoeff_inf( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       -1.39_WP , & ! ncoeff_inf( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       0.43_WP , & ! ncoeff_inf( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       -2.36_WP , & ! ncoeff_inf( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       -0.79_WP , & ! ncoeff_inf( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       -1.51_WP , & ! ncoeff_inf( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       1.45_WP , & ! ncoeff_inf( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1.16_WP , & ! ncoeff_inf( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       -1.59_WP , & ! ncoeff_inf( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.38_WP  & ! ncoeff_inf( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! Arrhenius parameters - Activation Energy 
  real(WP), parameter, dimension(npR+nPDR) :: Eact =(/ &
       2.6192e+04_WP , & ! Eact( r1f ) : # O+H2<=>H+OH # 
       -8.8280e+03_WP , & ! Eact( r2f ) : # 2OH<=>O+H2O # 
       1.3800e+04_WP , & ! Eact( r3f ) : # OH+H2<=>H2O+H # 
       6.2100e+04_WP , & ! Eact( r7f ) : # H+O2<=>OH+O # 
       0.0000e+00_WP , & ! Eact( r8 ) : # HO2+O=>OH+O2 # 
       5.0208e+04_WP , & ! Eact( r9 ) : # 2HO2=>O2+H2O2 # 
       1.2340e+03_WP , & ! Eact( r10 ) : # HO2+H=>2OH # 
       -6.8200e+03_WP , & ! Eact( r11 ) : # 2HO2=>O2+H2O2 # 
       -2.1000e+03_WP , & ! Eact( r12f ) : # HO2+OH<=>H2O+O2 # 
       -4.5720e+03_WP , & ! Eact( r13 ) : # H+HO2=>O2+H2 # 
       1.7870e+03_WP , & ! Eact( r14 ) : # H2O2+OH=>HO2+H2O # 
       1.5732e+05_WP , & ! Eact( r15f ) : # H2O2+OH<=>HO2+H2O # 
       0.0000e+00_WP , & ! Eact( r16 ) : # CH2+O=>HCO+H # 
       6.2760e+03_WP , & ! Eact( r17 ) : # CH2+O2=>HCO+OH # 
       0.0000e+00_WP , & ! Eact( r18 ) : # CH2+OH=>CH2O+H # 
       6.2760e+03_WP , & ! Eact( r19 ) : # CH2+O2=>CO2+2H # 
       0.0000e+00_WP , & ! Eact( r20f ) : # CH2D+H2<=>CH3+H # 
       0.0000e+00_WP , & ! Eact( r21 ) : # CH2D+O2=>H+OH+CO # 
       0.0000e+00_WP , & ! Eact( r22 ) : # CH2D+O2=>CO+H2O # 
       0.0000e+00_WP , & ! Eact( r23f ) : # CH2D+H2O<=>CH2+H2O # 
       0.0000e+00_WP , & ! Eact( r25 ) : # CH3+HO2=>CH4+O2 # 
       2.2677e+04_WP , & ! Eact( r26f ) : # CH3+OH<=>CH2+H2O # 
       0.0000e+00_WP , & ! Eact( r27 ) : # CH3+CH2=>C2H4+H # 
       0.0000e+00_WP , & ! Eact( r28 ) : # CH3+HO2=>CH3O+OH # 
       0.0000e+00_WP , & ! Eact( r29f ) : # CH3+OH<=>CH2D+H2O # 
       1.2050e+05_WP , & ! Eact( r30 ) : # CH3+O2=>O+CH3O # 
       3.7405e+04_WP , & ! Eact( r31 ) : # CH3+O2=>OH+CH2O # 
       0.0000e+00_WP , & ! Eact( r32 ) : # CH3+O=>CH2O+H # 
       1.3054e+04_WP , & ! Eact( r34f ) : # CH4+OH<=>CH3+H2O # 
       4.5355e+04_WP , & ! Eact( r35f ) : # CH4+H<=>CH3+H2 # 
       3.5982e+04_WP , & ! Eact( r36 ) : # CH4+O=>CH3+OH # 
       -1.4880e+03_WP , & ! Eact( r37f ) : # CO+OH<=>CO2+H # 
       7.5072e+04_WP , & ! Eact( r38 ) : # CO+HO2=>CO2+OH # 
       1.3880e+03_WP , & ! Eact( r39f ) : # CO+OH<=>CO2+H # 
       3.7303e+04_WP , & ! Eact( r40 ) : # CO+CH2=>CO+CH2D # 
       0.0000e+00_WP , & ! Eact( r41 ) : # HCO+OH=>CO+H2O # 
       0.0000e+00_WP , & ! Eact( r42 ) : # HCO+O=>CO2+H # 
       7.1128e+04_WP , & ! Eact( r43f ) : # HCO+H2O<=>CO+H+H2O # 
       0.0000e+00_WP , & ! Eact( r44 ) : # HCO+H=>CO+H2 # 
       0.0000e+00_WP , & ! Eact( r45 ) : # HCO+O=>CO+OH # 
       -3.0420e+03_WP , & ! Eact( r47 ) : # HCO+O2=>CO+HO2 # 
       3.3472e+04_WP , & ! Eact( r48 ) : # CH2O+HO2=>HCO+H2O2 # 
       1.3703e+04_WP , & ! Eact( r49 ) : # CH2O+H=>HCO+H2 # 
       1.6736e+05_WP , & ! Eact( r50 ) : # CH2O+O2=>HCO+HO2 # 
       -1.8700e+03_WP , & ! Eact( r51 ) : # CH2O+OH=>HCO+H2O # 
       2.4518e+04_WP , & ! Eact( r52 ) : # CH3+CH2O=>CH4+HCO # 
       1.4811e+04_WP , & ! Eact( r53 ) : # CH2O+O=>HCO+OH # 
       -1.4770e+04_WP , & ! Eact( r54 ) : # CH3O+O2=>CH2O+HO2 # 
       0.0000e+00_WP , & ! Eact( r56 ) : # CH2D+CO2=>CH2O+CO # 
       2.5104e+04_WP , & ! Eact( r57f ) : # C2H2+HCO<=>C2H3+CO # 
       7.9500e+03_WP , & ! Eact( r58 ) : # C2H2+O=>CH2+CO # 
       0.0000e+00_WP , & ! Eact( r59 ) : # C2H3+H=>C2H2+H2 # 
       4.5982e+01_WP , & ! Eact( r60 ) : # C2H3+O2=>CH2CHO+O # 
       4.2260e+03_WP , & ! Eact( r61 ) : # C2H3+O2=>HCO+CH2O # 
       -1.6040e+03_WP , & ! Eact( r62 ) : # C2H3+O2=>C2H2+HO2 # 
       0.0000e+00_WP , & ! Eact( r63 ) : # C2H3+OH=>C2H2+H2O # 
       0.0000e+00_WP , & ! Eact( r65 ) : # C2H3+O=>CH3+CO # 
       1.0460e+04_WP , & ! Eact( r67 ) : # C2H4+OH=>C2H3+H2O # 
       5.4183e+04_WP , & ! Eact( r68 ) : # C2H4+H=>C2H3+H2 # 
       1.5648e+04_WP , & ! Eact( r69 ) : # C2H4+O=>C2H3+OH # 
       3.3472e+04_WP , & ! Eact( r71 ) : # C2H4+HCO=>C2H5+CO # 
       9.2002e+02_WP , & ! Eact( r72 ) : # C2H4+O=>CH3+HCO # 
       0.0000e+00_WP , & ! Eact( r73 ) : # C2H5+HO2=>CH3+CH2O+OH # 
       7.5130e+03_WP , & ! Eact( r74 ) : # C2H5+H=>2CH3 # 
       0.0000e+00_WP , & ! Eact( r75 ) : # C2H5+O2=>C2H4+HO2 # 
       3.6400e+03_WP , & ! Eact( r76 ) : # C2H6+OH=>C2H5+H2O # 
       3.1506e+04_WP , & ! Eact( r77 ) : # C2H6+H=>C2H5+H2 # 
       0.0000e+00_WP , & ! Eact( r78 ) : # CH2CHO+H=>CH3+HCO # 
       0.0000e+00_WP , & ! Eact( r79 ) : # CH2CHO+O2=>CH2O+CO+OH # 
       1.9623e+05_WP , & ! Eact( r80 ) : # CH2CHO=>CH3+CO # 
       0.0000e+00_WP , & ! Eact( r83 ) : # A-C3H5+HO2=>OH+C2H3+CH2O # 
       0.0000e+00_WP , & ! Eact( r84 ) : # A-C3H5+HO2=>C3H6+O2 # 
       1.4195e+05_WP , & ! Eact( r85 ) : # H+A-C3H5=>CH3+C2H3 # 
       0.0000e+00_WP , & ! Eact( r86 ) : # A-C3H5+O=>C2H3CHO+H # 
       4.6777e+04_WP , & ! Eact( r87 ) : # C3H6+H=>C2H4+CH3 # 
       1.0418e+04_WP , & ! Eact( r88 ) : # C3H6+H=>A-C3H5+H2 # 
       -4.0670e+03_WP , & ! Eact( r89 ) : # C3H6+O=>C2H5+HCO # 
       1.3680e+03_WP , & ! Eact( r90 ) : # C3H6+O=>C2H3CHO+2H # 
       -1.2470e+03_WP , & ! Eact( r91 ) : # C3H6+OH=>A-C3H5+H2O # 
       1.2980e+05_WP , & ! Eact( r92f ) : # N-C3H7<=>CH3+C2H4 # 
       0.0000e+00_WP , & ! Eact( r93 ) : # N-C3H7+O2=>C3H6+HO2 # 
       0.0000e+00_WP , & ! Eact( r94 ) : # N-C3H7+HO2=>C2H5+OH+CH2O # 
       -1.8700e+03_WP , & ! Eact( r95 ) : # C2H3CHO+OH=>C2H3+H2O+CO # 
       0.0000e+00_WP , & ! Eact( r96 ) : # C4H7+HO2=>CH2O+OH+A-C3H5 # 
       2.1187e+05_WP , & ! Eact( r97 ) : # C4H7=>C2H3+C2H4 # 
       1.8702e+04_WP , & ! Eact( r98 ) : # C4H81+O=>C4H7+OH # 
       4.6777e+04_WP , & ! Eact( r99 ) : # C4H81+H=>C2H4+C2H5 # 
       -1.6820e+03_WP , & ! Eact( r100 ) : # C4H81+O=>N-C3H7+HCO # 
       2.8267e+04_WP , & ! Eact( r101 ) : # C4H81+H=>C4H7+H2 # 
       4.6777e+04_WP , & ! Eact( r102 ) : # C4H81+H=>C3H6+CH3 # 
       0.0000e+00_WP , & ! Eact( r103 ) : # P-C4H9+CH3=>C4H81+CH4 # 
       0.0000e+00_WP , & ! Eact( r104 ) : # P-C4H9+O=>N-C3H7+CH2O # 
       5.2321e+04_WP , & ! Eact( r105 ) : # P-C4H9+H=>2C2H5 # 
       0.0000e+00_WP , & ! Eact( r106 ) : # P-C4H9+HO2=>N-C3H7+OH+CH2O # 
       0.0000e+00_WP , & ! Eact( r107 ) : # P-C4H9+OH=>C4H81+H2O # 
       0.0000e+00_WP , & ! Eact( r109 ) : # P-C4H9+O2=>C4H81+HO2 # 
       4.6777e+04_WP , & ! Eact( r110 ) : # C5H10+H=>C3H6+C2H5 # 
       4.6777e+04_WP , & ! Eact( r111 ) : # C5H10+H=>C2H4+N-C3H7 # 
       1.2725e+05_WP , & ! Eact( r112 ) : # P-C5H11=>N-C3H7+C2H4 # 
       4.6777e+04_WP , & ! Eact( r113 ) : # C6H12+H=>C3H6+N-C3H7 # 
       4.6777e+04_WP , & ! Eact( r114 ) : # C6H12+H=>C2H4+P-C4H9 # 
       4.6777e+04_WP , & ! Eact( r115 ) : # C7H14+H=>C3H6+P-C4H9 # 
       4.6777e+04_WP , & ! Eact( r116 ) : # C7H14+H=>C2H4+P-C5H11 # 
       1.2787e+05_WP , & ! Eact( r117 ) : # P-C7H15=>P-C5H11+C2H4 # 
       4.6777e+04_WP , & ! Eact( r118 ) : # C8H16+H=>2C2H4+P-C4H9 # 
       4.6777e+04_WP , & ! Eact( r119 ) : # C8H16+H=>C3H6+P-C5H11 # 
       4.6777e+04_WP , & ! Eact( r121 ) : # C9H18+H=>C3H6+P-C4H9+C2H4 # 
       4.6777e+04_WP , & ! Eact( r122 ) : # C9H18+H=>C2H4+P-C7H15 # 
       4.6777e+04_WP , & ! Eact( r123 ) : # C10H20+H=>C3H6+P-C7H15 # 
       4.6777e+04_WP , & ! Eact( r124 ) : # C10H20+H=>3C2H4+P-C4H9 # 
       6.0250e+04_WP , & ! Eact( r125 ) : # P-C12H25=>S3-C12H25 # 
       1.2789e+05_WP , & ! Eact( r126 ) : # P-C12H25=>P-C4H9+4C2H4 # 
       0.0000e+00_WP , & ! Eact( r127 ) : # P-C12H25+O2=>C12H25O2 # 
       0.0000e+00_WP , & ! Eact( r128 ) : # S3-C12H25+O2=>C12H25O2 # 
       1.2671e+05_WP , & ! Eact( r129 ) : # S3-C12H25=>P-C4H9+C2H4+C6H12 # 
       1.2616e+05_WP , & ! Eact( r130 ) : # S3-C12H25=>C2H5+C10H20 # 
       1.2607e+05_WP , & ! Eact( r131 ) : # S3-C12H25=>N-C3H7+C9H18 # 
       1.2601e+05_WP , & ! Eact( r132 ) : # S3-C12H25=>P-C4H9+C8H16 # 
       1.2672e+05_WP , & ! Eact( r133 ) : # S3-C12H25=>P-C7H15+C5H10 # 
       1.2668e+05_WP , & ! Eact( r134 ) : # S3-C12H25=>P-C5H11+C7H14 # 
       1.2767e+05_WP , & ! Eact( r135 ) : # S-C12H25=>P-C7H15+C2H4+C3H6 # 
       0.0000e+00_WP , & ! Eact( r136 ) : # S-C12H25+O2=>C12H25O2 # 
       1.2638e+05_WP , & ! Eact( r137 ) : # S-C12H25=>P-C4H9+2C2H4+C4H81 # 
       3.7078e+05_WP , & ! Eact( r138 ) : # N-C12H26=>P-C5H11+P-C7H15 # 
       6.8994e+04_WP , & ! Eact( r139 ) : # N-C12H26+HO2=>P-C12H25+H2O2 # 
       3.7014e+05_WP , & ! Eact( r140 ) : # N-C12H26=>N-C3H7+P-C7H15+C2H4 # 
       2.9928e+04_WP , & ! Eact( r141 ) : # N-C12H26+CH3=>P-C12H25+CH4 # 
       8.8120e+03_WP , & ! Eact( r142 ) : # N-C12H26+O=>S-C12H25+OH # 
       5.8199e+04_WP , & ! Eact( r143 ) : # N-C12H26+HO2=>S3-C12H25+H2O2 # 
       2.8267e+04_WP , & ! Eact( r144 ) : # N-C12H26+H=>P-C12H25+H2 # 
       3.7023e+05_WP , & ! Eact( r145 ) : # N-C12H26=>C2H5+P-C4H9+3C2H4 # 
       1.5548e+04_WP , & ! Eact( r146 ) : # N-C12H26+O=>P-C12H25+OH # 
       3.7009e+05_WP , & ! Eact( r147 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       2.2928e+04_WP , & ! Eact( r148 ) : # N-C12H26+CH3=>S-C12H25+CH4 # 
       1.6440e+03_WP , & ! Eact( r149 ) : # N-C12H26+OH=>S3-C12H25+H2O # 
       1.9912e+05_WP , & ! Eact( r150 ) : # N-C12H26+O2=>S3-C12H25+HO2 # 
       3.7078e+05_WP , & ! Eact( r151 ) : # N-C12H26=>2P-C4H9+2C2H4 # 
       2.2050e+03_WP , & ! Eact( r152 ) : # N-C12H26+OH=>P-C12H25+H2O # 
       1.8707e+04_WP , & ! Eact( r153 ) : # N-C12H26+H=>S-C12H25+H2 # 
       1.8707e+04_WP , & ! Eact( r154 ) : # N-C12H26+H=>S3-C12H25+H2 # 
       5.8199e+04_WP , & ! Eact( r155 ) : # N-C12H26+HO2=>S-C12H25+H2O2 # 
       2.2928e+04_WP , & ! Eact( r156 ) : # N-C12H26+CH3=>S3-C12H25+CH4 # 
       1.9912e+05_WP , & ! Eact( r157 ) : # N-C12H26+O2=>S-C12H25+HO2 # 
       8.8120e+03_WP , & ! Eact( r158 ) : # N-C12H26+O=>S3-C12H25+OH # 
       1.6440e+03_WP , & ! Eact( r159 ) : # N-C12H26+OH=>S-C12H25+H2O # 
       1.1464e+05_WP , & ! Eact( r160 ) : # C12H25O2=>S3-C12H25+O2 # 
       1.1464e+05_WP , & ! Eact( r161 ) : # C12H25O2=>S-C12H25+O2 # 
       1.1464e+05_WP , & ! Eact( r162 ) : # C12H25O2=>P-C12H25+O2 # 
       7.9496e+04_WP , & ! Eact( r163 ) : # C12H25O2=>C12OOH # 
       4.8116e+04_WP , & ! Eact( r164 ) : # C12OOH=>C12H25O2 # 
       0.0000e+00_WP , & ! Eact( r165 ) : # C12OOH+O2=>O2C12H24OOH # 
       1.7600e+05_WP , & ! Eact( r166 ) : # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH # 
       7.1128e+04_WP , & ! Eact( r167 ) : # O2C12H24OOH=>OC12H23OOH+OH # 
       1.1464e+05_WP , & ! Eact( r168 ) : # O2C12H24OOH=>C12OOH+O2 # 
       2.5100e+03_WP , & ! Eact( r169f ) : # CH2D+N2<=>CH2+N2 # 
       2.0103e+04_WP , & ! Eact( r1b ) : # REV - O+H2<=>H+OH # 
       6.0306e+04_WP , & ! Eact( r2b ) : # REV - 2OH<=>O+H2O # 
       7.6845e+04_WP , & ! Eact( r3b ) : # REV - OH+H2<=>H2O+H # 
       -8.7204e+03_WP , & ! Eact( r7b ) : # REV - H+O2<=>OH+O # 
       2.8937e+05_WP , & ! Eact( r12b ) : # REV - HO2+OH<=>H2O+O2 # 
       2.8720e+05_WP , & ! Eact( r15b ) : # REV - H2O2+OH<=>HO2+H2O # 
       6.8303e+04_WP , & ! Eact( r20b ) : # REV - CH2D+H2<=>CH3+H # 
       3.7303e+04_WP , & ! Eact( r23b ) : # REV - CH2D+H2O<=>CH2+H2O # 
       5.4721e+04_WP , & ! Eact( r26b ) : # REV - CH3+OH<=>CH2+H2O # 
       -5.2588e+03_WP , & ! Eact( r29b ) : # REV - CH3+OH<=>CH2D+H2O # 
       6.7035e+04_WP , & ! Eact( r34b ) : # REV - CH4+OH<=>CH3+H2O # 
       3.6291e+04_WP , & ! Eact( r35b ) : # REV - CH4+H<=>CH3+H2 # 
       1.0621e+05_WP , & ! Eact( r37b ) : # REV - CO+OH<=>CO2+H # 
       1.0909e+05_WP , & ! Eact( r39b ) : # REV - CO+OH<=>CO2+H # 
       5.3544e+03_WP , & ! Eact( r43b ) : # REV - HCO+H2O<=>CO+H+H2O # 
       1.0613e+05_WP , & ! Eact( r57b ) : # REV - C2H2+HCO<=>C2H3+CO # 
       2.3788e+04_WP , & ! Eact( r92b ) : # REV - N-C3H7<=>CH3+C2H4 # 
       3.9813e+04_WP , & ! Eact( r169b ) : # REV - CH2D+N2<=>CH2+N2 # 
       0.0000e+00_WP , & ! Eact( r4f ) : # H+OH+M<=>H2O+M # 
       7.1128e+04_WP , & ! Eact( r46f ) : # HCO+M<=>CO+H+M # 
       4.9614e+05_WP , & ! Eact( r4b ) : # REV - H+OH+M<=>H2O+M # 
       5.3544e+03_WP , & ! Eact( r46b ) : # REV - HCO+M<=>CO+H+M # 
       -9.594e+03_WP , & ! Eact_0( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.000e+00_WP , & ! Eact_0( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       2.602e+04_WP , & ! Eact_0( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       1.021e+04_WP , & ! Eact_0( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       1.137e+05_WP , & ! Eact_0( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       4.088e+04_WP , & ! Eact_0( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       1.498e+05_WP , & ! Eact_0( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       3.160e+03_WP , & ! Eact_0( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       2.615e+04_WP , & ! Eact_0( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       2.497e+04_WP , & ! Eact_0( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1.685e+05_WP , & ! Eact_0( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       2.929e+04_WP , & ! Eact_0( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       2.0514e+05_WP , & ! Eact_0( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       2.0467e+05_WP , & ! Eact_0( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       4.1086e+05_WP , & ! Eact_0( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       4.5237e+05_WP , & ! Eact_0( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       4.7294e+05_WP , & ! Eact_0( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       2.9988e+03_WP , & ! Eact_0( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1.5450e+05_WP , & ! Eact_0( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       3.5003e+05_WP , & ! Eact_0( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       3.9298e+05_WP , & ! Eact_0( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
       0.0000e+00_WP , & ! Eact_inf( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.0000e+00_WP , & ! Eact_inf( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       2.5940e+03_WP , & ! Eact_inf( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       1.6020e+03_WP , & ! Eact_inf( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       1.0136e+05_WP , & ! Eact_inf( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       0.0000e+00_WP , & ! Eact_inf( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       1.5501e+05_WP , & ! Eact_inf( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       5.4000e+03_WP , & ! Eact_inf( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       -1.0970e+03_WP , & ! Eact_inf( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.0000e+00_WP , & ! Eact_inf( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1.5288e+05_WP , & ! Eact_inf( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1.3643e+04_WP , & ! Eact_inf( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       2.1474e+05_WP , & ! Eact_inf( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       2.0467e+05_WP , & ! Eact_inf( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       3.8743e+05_WP , & ! Eact_inf( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       4.4376e+05_WP , & ! Eact_inf( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       4.3206e+05_WP , & ! Eact_inf( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       8.2088e+03_WP , & ! Eact_inf( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1.5674e+05_WP , & ! Eact_inf( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       3.2278e+05_WP , & ! Eact_inf( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       3.6801e+05_WP  & ! Eact_inf( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fca for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fca =(/ &
       0.265400000000_WP , & ! fca( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.500000000000_WP , & ! fca( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       0.467500000000_WP , & ! fca( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       0.217000000000_WP , & ! fca( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       0.242000000000_WP , & ! fca( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       0.825000000000_WP , & ! fca( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       -0.982000000000_WP , & ! fca( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       0.240000000000_WP , & ! fca( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       0.896000000000_WP , & ! fca( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.980000000000_WP , & ! fca( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       0.000000000000_WP , & ! fca( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       0.000000000000_WP , & ! fca( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       0.265400000000_WP , & ! fca( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       0.500000000000_WP , & ! fca( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       0.467500000000_WP , & ! fca( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       0.217000000000_WP , & ! fca( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       0.825000000000_WP , & ! fca( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       -0.982000000000_WP , & ! fca( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       0.240000000000_WP , & ! fca( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       0.896000000000_WP , & ! fca( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.980000000000_WP  & ! fca( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fcta for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fcta =(/ &
       94.000000000000_WP , & ! fcta( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.000000000000_WP , & ! fcta( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       151.000000000000_WP , & ! fcta( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       74.000000000000_WP , & ! fcta( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       94.000000000000_WP , & ! fcta( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       1341.000000000000_WP , & ! fcta( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       5384.000000000000_WP , & ! fcta( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       40.000000000000_WP , & ! fcta( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       1606.000000000000_WP , & ! fcta( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1097.000000000000_WP , & ! fcta( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1000.000000000000_WP , & ! fcta( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1000.000000000000_WP , & ! fcta( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       94.000000000000_WP , & ! fcta( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       0.000000000000_WP , & ! fcta( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       151.000000000000_WP , & ! fcta( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       74.000000000000_WP , & ! fcta( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       1341.000000000000_WP , & ! fcta( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       5384.000000000000_WP , & ! fcta( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       40.000000000000_WP , & ! fcta( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       1606.000000000000_WP , & ! fcta( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1097.000000000000_WP  & ! fcta( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fcb for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fcb =(/ &
       0.734600000000_WP , & ! fcb( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       0.500000000000_WP , & ! fcb( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       0.532500000000_WP , & ! fcb( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       0.783000000000_WP , & ! fcb( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       0.758000000000_WP , & ! fcb( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       0.175000000000_WP , & ! fcb( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       1.982000000000_WP , & ! fcb( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       0.760000000000_WP , & ! fcb( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       0.104000000000_WP , & ! fcb( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.020000000000_WP , & ! fcb( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1.000000000000_WP , & ! fcb( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1.000000000000_WP , & ! fcb( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       0.734600000000_WP , & ! fcb( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       0.500000000000_WP , & ! fcb( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       0.532500000000_WP , & ! fcb( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       0.783000000000_WP , & ! fcb( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       0.175000000000_WP , & ! fcb( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       1.982000000000_WP , & ! fcb( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       0.760000000000_WP , & ! fcb( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       0.104000000000_WP , & ! fcb( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       0.980000000000_WP  & ! fcb( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fctb for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fctb =(/ &
       1756.000000000000_WP , & ! fctb( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctb( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       1038.000000000000_WP , & ! fctb( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       2941.000000000000_WP , & ! fctb( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       1555.000000000000_WP , & ! fctb( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       60000.000000000000_WP , & ! fctb( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       4.290000000000_WP , & ! fctb( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       1025.000000000000_WP , & ! fctb( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       60000.000000000000_WP , & ! fctb( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1097.000000000000_WP , & ! fctb( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1310.000000000000_WP , & ! fctb( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1310.000000000000_WP , & ! fctb( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       1756.000000000000_WP , & ! fctb( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctb( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       1038.000000000000_WP , & ! fctb( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       2941.000000000000_WP , & ! fctb( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       60000.000000000000_WP , & ! fctb( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       4.290000000000_WP , & ! fctb( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1025.000000000000_WP , & ! fctb( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       60000.000000000000_WP , & ! fctb( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1097.000000000000_WP  & ! fctb( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fcc for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fcc =(/ &
       1.000000000000_WP , & ! fcc( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       1.000000000000_WP , & ! fcc( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       1.000000000000_WP , & ! fcc( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       1.000000000000_WP , & ! fcc( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       1.000000000000_WP , & ! fcc( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       1.000000000000_WP , & ! fcc( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       1.000000000000_WP , & ! fcc( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       1.000000000000_WP , & ! fcc( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       1.000000000000_WP , & ! fcc( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1.000000000000_WP , & ! fcc( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       1.000000000000_WP , & ! fcc( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       1.000000000000_WP , & ! fcc( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       1.000000000000_WP , & ! fcc( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       1.000000000000_WP , & ! fcc( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       1.000000000000_WP , & ! fcc( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       1.000000000000_WP , & ! fcc( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       1.000000000000_WP , & ! fcc( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       1.000000000000_WP , & ! fcc( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1.000000000000_WP , & ! fcc( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       1.000000000000_WP , & ! fcc( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       1.000000000000_WP  & ! fcc( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! fctc for pressure dependent reactions
  real(WP), parameter, dimension(npRL+1:npR) :: fctc =(/ &
       5182.000000000000_WP , & ! fctc( r5f ) : # 2OH(+M)<=>H2O2(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctc( r6f ) : # H+O2(+M)<=>HO2(+M) # 
       4970.000000000000_WP , & ! fctc( r24f ) : # 2CH3(+M)<=>C2H6(+M) # 
       6964.000000000000_WP , & ! fctc( r33f ) : # CH3+H(+M)<=>CH4(+M) # 
       4200.000000000000_WP , & ! fctc( r55 ) : # CH3O(+M)=>H+CH2O(+M) # 
       10140.000000000000_WP , & ! fctc( r64f ) : # C2H3+CH3(+M)<=>C3H6(+M) # 
       -0.080000000000_WP , & ! fctc( r66f ) : # C2H3(+M)<=>C2H2+H(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctc( r70f ) : # C2H4+H(+M)<=>C2H5(+M) # 
       6118.000000000000_WP , & ! fctc( r81f ) : # A-C3H5+CH3(+M)<=>C4H81(+M) # 
       6860.000000000000_WP , & ! fctc( r82f ) : # A-C3H5+H(+M)<=>C3H6(+M) # 
       48100.000000000000_WP , & ! fctc( r108 ) : # P-C4H9(+M)=>H+C4H81(+M) # 
       48100.000000000000_WP , & ! fctc( r120 ) : # C8H16+H(+M)=>P-C4H9+2C2H4(+M) # 
       5182.000000000000_WP , & ! fctc( r5b ) : # REV - 2OH(+M)<=>H2O2(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctc( r6b ) : # REV - H+O2(+M)<=>HO2(+M) # 
       4970.000000000000_WP , & ! fctc( r24b ) : # REV - 2CH3(+M)<=>C2H6(+M) # 
       6964.000000000000_WP , & ! fctc( r33b ) : # REV - CH3+H(+M)<=>CH4(+M) # 
       10140.000000000000_WP , & ! fctc( r64b ) : # REV - C2H3+CH3(+M)<=>C3H6(+M) # 
       -0.080000000000_WP , & ! fctc( r66b ) : # REV - C2H3(+M)<=>C2H2+H(+M) # 
       1000000000000000019884624838656.000000000000_WP , & ! fctc( r70b ) : # REV - C2H4+H(+M)<=>C2H5(+M) # 
       6118.000000000000_WP , & ! fctc( r81b ) : # REV - A-C3H5+CH3(+M)<=>C4H81(+M) # 
       6860.000000000000_WP  & ! fctc( r82b ) : # REV - A-C3H5+H(+M)<=>C3H6(+M) # 
  /)

  ! Reaction definition and Stoichiometric coefficients 

  ! Reactant species
  integer, parameter, dimension(nreactants_max,npR) :: reactants = reshape( (/ &
       gO ,  gH2 ,  -1  , & ! Reaction # O+H2<=>H+OH #
       gOH ,  -1  ,  -1  , & ! Reaction # 2OH<=>O+H2O #
       gOH ,  gH2 ,  -1  , & ! Reaction # OH+H2<=>H2O+H #
       gH ,  gO2 ,  -1  , & ! Reaction # H+O2<=>OH+O #
       gO ,  gHO2 ,  -1  , & ! Reaction # HO2+O=>OH+O2 #
       gHO2 ,  -1  ,  -1  , & ! Reaction # 2HO2=>O2+H2O2 #
       gH ,  gHO2 ,  -1  , & ! Reaction # HO2+H=>2OH #
       gHO2 ,  -1  ,  -1  , & ! Reaction # 2HO2=>O2+H2O2 #
       gHO2 ,  gOH ,  -1  , & ! Reaction # HO2+OH<=>H2O+O2 #
       gHO2 ,  gH ,  -1  , & ! Reaction # H+HO2=>O2+H2 #
       gH2O2 ,  gOH ,  -1  , & ! Reaction # H2O2+OH=>HO2+H2O #
       gOH ,  gH2O2 ,  -1  , & ! Reaction # H2O2+OH<=>HO2+H2O #
       gCH2 ,  gO ,  -1  , & ! Reaction # CH2+O=>HCO+H #
       gCH2 ,  gO2 ,  -1  , & ! Reaction # CH2+O2=>HCO+OH #
       gCH2 ,  gOH ,  -1  , & ! Reaction # CH2+OH=>CH2O+H #
       gO2 ,  gCH2 ,  -1  , & ! Reaction # CH2+O2=>CO2+2H #
       gH2 ,  gCH2D ,  -1  , & ! Reaction # CH2D+H2<=>CH3+H #
       gO2 ,  gCH2D ,  -1  , & ! Reaction # CH2D+O2=>H+OH+CO #
       gO2 ,  gCH2D ,  -1  , & ! Reaction # CH2D+O2=>CO+H2O #
       gH2O ,  gCH2D ,  -1  , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       gCH3 ,  gHO2 ,  -1  , & ! Reaction # CH3+HO2=>CH4+O2 #
       gOH ,  gCH3 ,  -1  , & ! Reaction # CH3+OH<=>CH2+H2O #
       gCH2 ,  gCH3 ,  -1  , & ! Reaction # CH3+CH2=>C2H4+H #
       gCH3 ,  gHO2 ,  -1  , & ! Reaction # CH3+HO2=>CH3O+OH #
       gCH3 ,  gOH ,  -1  , & ! Reaction # CH3+OH<=>CH2D+H2O #
       gCH3 ,  gO2 ,  -1  , & ! Reaction # CH3+O2=>O+CH3O #
       gCH3 ,  gO2 ,  -1  , & ! Reaction # CH3+O2=>OH+CH2O #
       gO ,  gCH3 ,  -1  , & ! Reaction # CH3+O=>CH2O+H #
       gCH4 ,  gOH ,  -1  , & ! Reaction # CH4+OH<=>CH3+H2O #
       gCH4 ,  gH ,  -1  , & ! Reaction # CH4+H<=>CH3+H2 #
       gO ,  gCH4 ,  -1  , & ! Reaction # CH4+O=>CH3+OH #
       gCO ,  gOH ,  -1  , & ! Reaction # CO+OH<=>CO2+H #
       gHO2 ,  gCO ,  -1  , & ! Reaction # CO+HO2=>CO2+OH #
       gOH ,  gCO ,  -1  , & ! Reaction # CO+OH<=>CO2+H #
       gCH2 ,  gCO ,  -1  , & ! Reaction # CO+CH2=>CO+CH2D #
       gHCO ,  gOH ,  -1  , & ! Reaction # HCO+OH=>CO+H2O #
       gO ,  gHCO ,  -1  , & ! Reaction # HCO+O=>CO2+H #
       gH2O ,  gHCO ,  -1  , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       gH ,  gHCO ,  -1  , & ! Reaction # HCO+H=>CO+H2 #
       gO ,  gHCO ,  -1  , & ! Reaction # HCO+O=>CO+OH #
       gO2 ,  gHCO ,  -1  , & ! Reaction # HCO+O2=>CO+HO2 #
       gHO2 ,  gCH2O ,  -1  , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       gH ,  gCH2O ,  -1  , & ! Reaction # CH2O+H=>HCO+H2 #
       gCH2O ,  gO2 ,  -1  , & ! Reaction # CH2O+O2=>HCO+HO2 #
       gOH ,  gCH2O ,  -1  , & ! Reaction # CH2O+OH=>HCO+H2O #
       gCH3 ,  gCH2O ,  -1  , & ! Reaction # CH3+CH2O=>CH4+HCO #
       gCH2O ,  gO ,  -1  , & ! Reaction # CH2O+O=>HCO+OH #
       gO2 ,  gCH3O ,  -1  , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       gCH2D ,  gCO2 ,  -1  , & ! Reaction # CH2D+CO2=>CH2O+CO #
       gC2H2 ,  gHCO ,  -1  , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       gO ,  gC2H2 ,  -1  , & ! Reaction # C2H2+O=>CH2+CO #
       gH ,  gC2H3 ,  -1  , & ! Reaction # C2H3+H=>C2H2+H2 #
       gC2H3 ,  gO2 ,  -1  , & ! Reaction # C2H3+O2=>CH2CHO+O #
       gO2 ,  gC2H3 ,  -1  , & ! Reaction # C2H3+O2=>HCO+CH2O #
       gC2H3 ,  gO2 ,  -1  , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       gC2H3 ,  gOH ,  -1  , & ! Reaction # C2H3+OH=>C2H2+H2O #
       gO ,  gC2H3 ,  -1  , & ! Reaction # C2H3+O=>CH3+CO #
       gC2H4 ,  gOH ,  -1  , & ! Reaction # C2H4+OH=>C2H3+H2O #
       gH ,  gC2H4 ,  -1  , & ! Reaction # C2H4+H=>C2H3+H2 #
       gC2H4 ,  gO ,  -1  , & ! Reaction # C2H4+O=>C2H3+OH #
       gC2H4 ,  gHCO ,  -1  , & ! Reaction # C2H4+HCO=>C2H5+CO #
       gC2H4 ,  gO ,  -1  , & ! Reaction # C2H4+O=>CH3+HCO #
       gC2H5 ,  gHO2 ,  -1  , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       gH ,  gC2H5 ,  -1  , & ! Reaction # C2H5+H=>2CH3 #
       gC2H5 ,  gO2 ,  -1  , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       gOH ,  gC2H6 ,  -1  , & ! Reaction # C2H6+OH=>C2H5+H2O #
       gC2H6 ,  gH ,  -1  , & ! Reaction # C2H6+H=>C2H5+H2 #
       gH ,  gCH2CHO ,  -1  , & ! Reaction # CH2CHO+H=>CH3+HCO #
       gCH2CHO ,  gO2 ,  -1  , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       gCH2CHO ,  -1  ,  -1  , & ! Reaction # CH2CHO=>CH3+CO #
       gAXC3H5 ,  gHO2 ,  -1  , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       gHO2 ,  gAXC3H5 ,  -1  , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       gH ,  gAXC3H5 ,  -1  , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       gO ,  gAXC3H5 ,  -1  , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       gH ,  gC3H6 ,  -1  , & ! Reaction # C3H6+H=>C2H4+CH3 #
       gH ,  gC3H6 ,  -1  , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       gC3H6 ,  gO ,  -1  , & ! Reaction # C3H6+O=>C2H5+HCO #
       gC3H6 ,  gO ,  -1  , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       gOH ,  gC3H6 ,  -1  , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       gNXC3H7 ,  -1  ,  -1  , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       gO2 ,  gNXC3H7 ,  -1  , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       gNXC3H7 ,  gHO2 ,  -1  , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       gC2H3CHO ,  gOH ,  -1  , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       gHO2 ,  gC4H7 ,  -1  , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       gC4H7 ,  -1  ,  -1  , & ! Reaction # C4H7=>C2H3+C2H4 #
       gO ,  gC4H81 ,  -1  , & ! Reaction # C4H81+O=>C4H7+OH #
       gH ,  gC4H81 ,  -1  , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       gO ,  gC4H81 ,  -1  , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       gH ,  gC4H81 ,  -1  , & ! Reaction # C4H81+H=>C4H7+H2 #
       gC4H81 ,  gH ,  -1  , & ! Reaction # C4H81+H=>C3H6+CH3 #
       gCH3 ,  gPXC4H9 ,  -1  , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       gO ,  gPXC4H9 ,  -1  , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       gPXC4H9 ,  gH ,  -1  , & ! Reaction # P-C4H9+H=>2C2H5 #
       gPXC4H9 ,  gHO2 ,  -1  , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       gOH ,  gPXC4H9 ,  -1  , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       gO2 ,  gPXC4H9 ,  -1  , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       gC5H10 ,  gH ,  -1  , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       gC5H10 ,  gH ,  -1  , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       gPXC5H11 ,  -1  ,  -1  , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       gH ,  gC6H12 ,  -1  , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       gC6H12 ,  gH ,  -1  , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       gH ,  gC7H14 ,  -1  , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       gH ,  gC7H14 ,  -1  , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       gPXC7H15 ,  -1  ,  -1  , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       gC8H16 ,  gH ,  -1  , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       gC8H16 ,  gH ,  -1  , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       gH ,  gC9H18 ,  -1  , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       gH ,  gC9H18 ,  -1  , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       gH ,  gC10H20 ,  -1  , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       gH ,  gC10H20 ,  -1  , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       gPXC12H25 ,  -1  ,  -1  , & ! Reaction # P-C12H25=>S3-C12H25 #
       gPXC12H25 ,  -1  ,  -1  , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       gPXC12H25 ,  gO2 ,  -1  , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       gO2 ,  gS3XC12H25 ,  -1  , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       gS3XC12H25 ,  -1  ,  -1  , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       gSXC12H25 ,  -1  ,  -1  , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       gSXC12H25 ,  gO2 ,  -1  , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       gSXC12H25 ,  -1  ,  -1  , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       gNXC12H26 ,  -1  ,  -1  , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       gHO2 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       gNXC12H26 ,  -1  ,  -1  , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       gCH3 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       gO ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       gNXC12H26 ,  gHO2 ,  -1  , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       gH ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       gNXC12H26 ,  -1  ,  -1  , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       gNXC12H26 ,  gO ,  -1  , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       gNXC12H26 ,  -1  ,  -1  , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       gCH3 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       gNXC12H26 ,  gOH ,  -1  , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       gNXC12H26 ,  gO2 ,  -1  , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       gNXC12H26 ,  -1  ,  -1  , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       gOH ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       gH ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       gNXC12H26 ,  gH ,  -1  , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       gHO2 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       gCH3 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       gO2 ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       gO ,  gNXC12H26 ,  -1  , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       gNXC12H26 ,  gOH ,  -1  , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       gC12H25O2 ,  -1  ,  -1  , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       gC12H25O2 ,  -1  ,  -1  , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       gC12H25O2 ,  -1  ,  -1  , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       gC12H25O2 ,  -1  ,  -1  , & ! Reaction # C12H25O2=>C12OOH #
       gC12OOH ,  -1  ,  -1  , & ! Reaction # C12OOH=>C12H25O2 #
       gO2 ,  gC12OOH ,  -1  , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       gOC12H23OOH ,  -1  ,  -1  , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       gO2C12H24OOH ,  -1  ,  -1  , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       gO2C12H24OOH ,  -1  ,  -1  , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       gCH2D ,  gN2 ,  -1  , & ! Reaction # CH2D+N2<=>CH2+N2 #
       gOH ,  gH ,  -1  , & ! Reaction # REV - O+H2<=>H+OH #
       gO ,  gH2O ,  -1  , & ! Reaction # REV - 2OH<=>O+H2O #
       gH ,  gH2O ,  -1  , & ! Reaction # REV - OH+H2<=>H2O+H #
       gOH ,  gO ,  -1  , & ! Reaction # REV - H+O2<=>OH+O #
       gH2O ,  gO2 ,  -1  , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       gH2O ,  gHO2 ,  -1  , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       gH ,  gCH3 ,  -1  , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       gH2O ,  gCH2 ,  -1  , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       gH2O ,  gCH2 ,  -1  , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       gH2O ,  gCH2D ,  -1  , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       gCH3 ,  gH2O ,  -1  , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       gCH3 ,  gH2 ,  -1  , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       gCO2 ,  gH ,  -1  , & ! Reaction # REV - CO+OH<=>CO2+H #
       gH ,  gCO2 ,  -1  , & ! Reaction # REV - CO+OH<=>CO2+H #
       gH2O ,  gH ,  gCO , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       gC2H3 ,  gCO ,  -1  , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       gC2H4 ,  gCH3 ,  -1  , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       gN2 ,  gCH2 ,  -1  , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       gOH ,  gH , mM1 , & ! Reaction # H+OH+M<=>H2O+M #
       gHCO , mM6 ,  -1  , & ! Reaction # HCO+M<=>CO+H+M #
       gH2O , mM1 ,  -1  , & ! Reaction # REV - H+OH+M<=>H2O+M #
       gCO ,  gH , mM6 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       gOH , mM2 ,  -1  , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       gO2 ,  gH , mM3 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       gCH3 , mM4 ,  -1  , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       gCH3 ,  gH , mM5 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       gCH3O , mM7 ,  -1  , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       gC2H3 ,  gCH3 , mM8 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       gC2H3 , mM9 ,  -1  , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       gC2H4 ,  gH , mM10 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       gAXC3H5 ,  gCH3 , mM11 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       gH ,  gAXC3H5 , mM12 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       gPXC4H9 , mM13 ,  -1  , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       gC8H16 ,  gH , mM14 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       gH2O2 , mM2 ,  -1  , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       gHO2 , mM3 ,  -1  , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       gC2H6 , mM4 ,  -1  , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       gCH4 , mM5 ,  -1  , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       gC3H6 , mM8 ,  -1  , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       gH ,  gC2H2 , mM9 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       gC2H5 , mM10 ,  -1  , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       gC4H81 , mM11 ,  -1  , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       gC3H6 , mM12 ,  -1  & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /), (/nreactants_max,npR/) )

  ! Product species
  integer, parameter, dimension(nproducts_max,npR) :: products = reshape( (/ &
       gOH ,  gH ,  -1 ,  -1 , & ! Reaction # O+H2<=>H+OH #
       gO ,  gH2O ,  -1 ,  -1 , & ! Reaction # 2OH<=>O+H2O #
       gH ,  gH2O ,  -1 ,  -1 , & ! Reaction # OH+H2<=>H2O+H #
       gOH ,  gO ,  -1 ,  -1 , & ! Reaction # H+O2<=>OH+O #
       gOH ,  gO2 ,  -1 ,  -1 , & ! Reaction # HO2+O=>OH+O2 #
       gH2O2 ,  gO2 ,  -1 ,  -1 , & ! Reaction # 2HO2=>O2+H2O2 #
       gOH ,  -1 ,  -1 ,  -1 , & ! Reaction # HO2+H=>2OH #
       gH2O2 ,  gO2 ,  -1 ,  -1 , & ! Reaction # 2HO2=>O2+H2O2 #
       gH2O ,  gO2 ,  -1 ,  -1 , & ! Reaction # HO2+OH<=>H2O+O2 #
       gO2 ,  gH2 ,  -1 ,  -1 , & ! Reaction # H+HO2=>O2+H2 #
       gHO2 ,  gH2O ,  -1 ,  -1 , & ! Reaction # H2O2+OH=>HO2+H2O #
       gH2O ,  gHO2 ,  -1 ,  -1 , & ! Reaction # H2O2+OH<=>HO2+H2O #
       gH ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2+O=>HCO+H #
       gHCO ,  gOH ,  -1 ,  -1 , & ! Reaction # CH2+O2=>HCO+OH #
       gCH2O ,  gH ,  -1 ,  -1 , & ! Reaction # CH2+OH=>CH2O+H #
       gH ,  gCO2 ,  -1 ,  -1 , & ! Reaction # CH2+O2=>CO2+2H #
       gH ,  gCH3 ,  -1 ,  -1 , & ! Reaction # CH2D+H2<=>CH3+H #
       gOH ,  gCO ,  gH ,  -1 , & ! Reaction # CH2D+O2=>H+OH+CO #
       gH2O ,  gCO ,  -1 ,  -1 , & ! Reaction # CH2D+O2=>CO+H2O #
       gH2O ,  gCH2 ,  -1 ,  -1 , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       gO2 ,  gCH4 ,  -1 ,  -1 , & ! Reaction # CH3+HO2=>CH4+O2 #
       gH2O ,  gCH2 ,  -1 ,  -1 , & ! Reaction # CH3+OH<=>CH2+H2O #
       gH ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # CH3+CH2=>C2H4+H #
       gOH ,  gCH3O ,  -1 ,  -1 , & ! Reaction # CH3+HO2=>CH3O+OH #
       gH2O ,  gCH2D ,  -1 ,  -1 , & ! Reaction # CH3+OH<=>CH2D+H2O #
       gCH3O ,  gO ,  -1 ,  -1 , & ! Reaction # CH3+O2=>O+CH3O #
       gOH ,  gCH2O ,  -1 ,  -1 , & ! Reaction # CH3+O2=>OH+CH2O #
       gCH2O ,  gH ,  -1 ,  -1 , & ! Reaction # CH3+O=>CH2O+H #
       gCH3 ,  gH2O ,  -1 ,  -1 , & ! Reaction # CH4+OH<=>CH3+H2O #
       gCH3 ,  gH2 ,  -1 ,  -1 , & ! Reaction # CH4+H<=>CH3+H2 #
       gCH3 ,  gOH ,  -1 ,  -1 , & ! Reaction # CH4+O=>CH3+OH #
       gCO2 ,  gH ,  -1 ,  -1 , & ! Reaction # CO+OH<=>CO2+H #
       gOH ,  gCO2 ,  -1 ,  -1 , & ! Reaction # CO+HO2=>CO2+OH #
       gH ,  gCO2 ,  -1 ,  -1 , & ! Reaction # CO+OH<=>CO2+H #
       gCH2D ,  gCO ,  -1 ,  -1 , & ! Reaction # CO+CH2=>CO+CH2D #
       gH2O ,  gCO ,  -1 ,  -1 , & ! Reaction # HCO+OH=>CO+H2O #
       gH ,  gCO2 ,  -1 ,  -1 , & ! Reaction # HCO+O=>CO2+H #
       gH2O ,  gH ,  gCO ,  -1 , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       gCO ,  gH2 ,  -1 ,  -1 , & ! Reaction # HCO+H=>CO+H2 #
       gCO ,  gOH ,  -1 ,  -1 , & ! Reaction # HCO+O=>CO+OH #
       gHO2 ,  gCO ,  -1 ,  -1 , & ! Reaction # HCO+O2=>CO+HO2 #
       gH2O2 ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       gH2 ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2O+H=>HCO+H2 #
       gHO2 ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2O+O2=>HCO+HO2 #
       gH2O ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2O+OH=>HCO+H2O #
       gCH4 ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH3+CH2O=>CH4+HCO #
       gHCO ,  gOH ,  -1 ,  -1 , & ! Reaction # CH2O+O=>HCO+OH #
       gCH2O ,  gHO2 ,  -1 ,  -1 , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       gCH2O ,  gCO ,  -1 ,  -1 , & ! Reaction # CH2D+CO2=>CH2O+CO #
       gC2H3 ,  gCO ,  -1 ,  -1 , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       gCH2 ,  gCO ,  -1 ,  -1 , & ! Reaction # C2H2+O=>CH2+CO #
       gC2H2 ,  gH2 ,  -1 ,  -1 , & ! Reaction # C2H3+H=>C2H2+H2 #
       gCH2CHO ,  gO ,  -1 ,  -1 , & ! Reaction # C2H3+O2=>CH2CHO+O #
       gCH2O ,  gHCO ,  -1 ,  -1 , & ! Reaction # C2H3+O2=>HCO+CH2O #
       gHO2 ,  gC2H2 ,  -1 ,  -1 , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       gC2H2 ,  gH2O ,  -1 ,  -1 , & ! Reaction # C2H3+OH=>C2H2+H2O #
       gCH3 ,  gCO ,  -1 ,  -1 , & ! Reaction # C2H3+O=>CH3+CO #
       gC2H3 ,  gH2O ,  -1 ,  -1 , & ! Reaction # C2H4+OH=>C2H3+H2O #
       gH2 ,  gC2H3 ,  -1 ,  -1 , & ! Reaction # C2H4+H=>C2H3+H2 #
       gC2H3 ,  gOH ,  -1 ,  -1 , & ! Reaction # C2H4+O=>C2H3+OH #
       gCO ,  gC2H5 ,  -1 ,  -1 , & ! Reaction # C2H4+HCO=>C2H5+CO #
       gCH3 ,  gHCO ,  -1 ,  -1 , & ! Reaction # C2H4+O=>CH3+HCO #
       gCH3 ,  gCH2O ,  gOH ,  -1 , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       gCH3 ,  -1 ,  -1 ,  -1 , & ! Reaction # C2H5+H=>2CH3 #
       gC2H4 ,  gHO2 ,  -1 ,  -1 , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       gC2H5 ,  gH2O ,  -1 ,  -1 , & ! Reaction # C2H6+OH=>C2H5+H2O #
       gH2 ,  gC2H5 ,  -1 ,  -1 , & ! Reaction # C2H6+H=>C2H5+H2 #
       gCH3 ,  gHCO ,  -1 ,  -1 , & ! Reaction # CH2CHO+H=>CH3+HCO #
       gCH2O ,  gOH ,  gCO ,  -1 , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       gCH3 ,  gCO ,  -1 ,  -1 , & ! Reaction # CH2CHO=>CH3+CO #
       gOH ,  gC2H3 ,  gCH2O ,  -1 , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       gO2 ,  gC3H6 ,  -1 ,  -1 , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       gCH3 ,  gC2H3 ,  -1 ,  -1 , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       gH ,  gC2H3CHO ,  -1 ,  -1 , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       gCH3 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # C3H6+H=>C2H4+CH3 #
       gAXC3H5 ,  gH2 ,  -1 ,  -1 , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       gHCO ,  gC2H5 ,  -1 ,  -1 , & ! Reaction # C3H6+O=>C2H5+HCO #
       gC2H3CHO ,  gH ,  -1 ,  -1 , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       gH2O ,  gAXC3H5 ,  -1 ,  -1 , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       gC2H4 ,  gCH3 ,  -1 ,  -1 , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       gC3H6 ,  gHO2 ,  -1 ,  -1 , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       gOH ,  gC2H5 ,  gCH2O ,  -1 , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       gC2H3 ,  gH2O ,  gCO ,  -1 , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       gAXC3H5 ,  gCH2O ,  gOH ,  -1 , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       gC2H3 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # C4H7=>C2H3+C2H4 #
       gOH ,  gC4H7 ,  -1 ,  -1 , & ! Reaction # C4H81+O=>C4H7+OH #
       gC2H4 ,  gC2H5 ,  -1 ,  -1 , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       gNXC3H7 ,  gHCO ,  -1 ,  -1 , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       gC4H7 ,  gH2 ,  -1 ,  -1 , & ! Reaction # C4H81+H=>C4H7+H2 #
       gC3H6 ,  gCH3 ,  -1 ,  -1 , & ! Reaction # C4H81+H=>C3H6+CH3 #
       gC4H81 ,  gCH4 ,  -1 ,  -1 , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       gCH2O ,  gNXC3H7 ,  -1 ,  -1 , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       gC2H5 ,  -1 ,  -1 ,  -1 , & ! Reaction # P-C4H9+H=>2C2H5 #
       gCH2O ,  gOH ,  gNXC3H7 ,  -1 , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       gC4H81 ,  gH2O ,  -1 ,  -1 , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       gC4H81 ,  gHO2 ,  -1 ,  -1 , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       gC2H5 ,  gC3H6 ,  -1 ,  -1 , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       gC2H4 ,  gNXC3H7 ,  -1 ,  -1 , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       gNXC3H7 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       gNXC3H7 ,  gC3H6 ,  -1 ,  -1 , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       gC2H4 ,  gPXC4H9 ,  -1 ,  -1 , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       gC3H6 ,  gPXC4H9 ,  -1 ,  -1 , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       gC2H4 ,  gPXC5H11 ,  -1 ,  -1 , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       gPXC5H11 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       gC2H4 ,  gPXC4H9 ,  -1 ,  -1 , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       gC3H6 ,  gPXC5H11 ,  -1 ,  -1 , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       gC3H6 ,  gPXC4H9 ,  gC2H4 ,  -1 , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       gPXC7H15 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       gPXC7H15 ,  gC3H6 ,  -1 ,  -1 , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       gPXC4H9 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       gS3XC12H25 ,  -1 ,  -1 ,  -1 , & ! Reaction # P-C12H25=>S3-C12H25 #
       gC2H4 ,  gPXC4H9 ,  -1 ,  -1 , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       gC12H25O2 ,  -1 ,  -1 ,  -1 , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       gC12H25O2 ,  -1 ,  -1 ,  -1 , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       gPXC4H9 ,  gC6H12 ,  gC2H4 ,  -1 , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       gC10H20 ,  gC2H5 ,  -1 ,  -1 , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       gNXC3H7 ,  gC9H18 ,  -1 ,  -1 , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       gC8H16 ,  gPXC4H9 ,  -1 ,  -1 , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       gC5H10 ,  gPXC7H15 ,  -1 ,  -1 , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       gC7H14 ,  gPXC5H11 ,  -1 ,  -1 , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       gC3H6 ,  gC2H4 ,  gPXC7H15 ,  -1 , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       gC12H25O2 ,  -1 ,  -1 ,  -1 , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       gC2H4 ,  gC4H81 ,  gPXC4H9 ,  -1 , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       gPXC5H11 ,  gPXC7H15 ,  -1 ,  -1 , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       gPXC12H25 ,  gH2O2 ,  -1 ,  -1 , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       gPXC7H15 ,  gNXC3H7 ,  gC2H4 ,  -1 , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       gCH4 ,  gPXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       gSXC12H25 ,  gOH ,  -1 ,  -1 , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       gS3XC12H25 ,  gH2O2 ,  -1 ,  -1 , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       gH2 ,  gPXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       gPXC4H9 ,  gC2H5 ,  gC2H4 ,  -1 , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       gOH ,  gPXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       gPXC4H9 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       gCH4 ,  gSXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       gS3XC12H25 ,  gH2O ,  -1 ,  -1 , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       gHO2 ,  gS3XC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       gPXC4H9 ,  gC2H4 ,  -1 ,  -1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       gH2O ,  gPXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       gSXC12H25 ,  gH2 ,  -1 ,  -1 , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       gS3XC12H25 ,  gH2 ,  -1 ,  -1 , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       gH2O2 ,  gSXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       gS3XC12H25 ,  gCH4 ,  -1 ,  -1 , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       gHO2 ,  gSXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       gS3XC12H25 ,  gOH ,  -1 ,  -1 , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       gH2O ,  gSXC12H25 ,  -1 ,  -1 , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       gS3XC12H25 ,  gO2 ,  -1 ,  -1 , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       gSXC12H25 ,  gO2 ,  -1 ,  -1 , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       gPXC12H25 ,  gO2 ,  -1 ,  -1 , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       gC12OOH ,  -1 ,  -1 ,  -1 , & ! Reaction # C12H25O2=>C12OOH #
       gC12H25O2 ,  -1 ,  -1 ,  -1 , & ! Reaction # C12OOH=>C12H25O2 #
       gO2C12H24OOH ,  -1 ,  -1 ,  -1 , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       gOH ,  gCH2CHO ,  gC2H4 ,  gC2H5 , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       gOC12H23OOH ,  gOH ,  -1 ,  -1 , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       gC12OOH ,  gO2 ,  -1 ,  -1 , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       gN2 ,  gCH2 ,  -1 ,  -1 , & ! Reaction # CH2D+N2<=>CH2+N2 #
       gO ,  gH2 ,  -1 ,  -1 , & ! Reaction # REV - O+H2<=>H+OH #
       gOH ,  -1 ,  -1 ,  -1 , & ! Reaction # REV - 2OH<=>O+H2O #
       gOH ,  gH2 ,  -1 ,  -1 , & ! Reaction # REV - OH+H2<=>H2O+H #
       gH ,  gO2 ,  -1 ,  -1 , & ! Reaction # REV - H+O2<=>OH+O #
       gHO2 ,  gOH ,  -1 ,  -1 , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       gOH ,  gH2O2 ,  -1 ,  -1 , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       gH2 ,  gCH2D ,  -1 ,  -1 , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       gH2O ,  gCH2D ,  -1 ,  -1 , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       gOH ,  gCH3 ,  -1 ,  -1 , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       gCH3 ,  gOH ,  -1 ,  -1 , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       gCH4 ,  gOH ,  -1 ,  -1 , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       gCH4 ,  gH ,  -1 ,  -1 , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       gCO ,  gOH ,  -1 ,  -1 , & ! Reaction # REV - CO+OH<=>CO2+H #
       gOH ,  gCO ,  -1 ,  -1 , & ! Reaction # REV - CO+OH<=>CO2+H #
       gH2O ,  gHCO ,  -1 ,  -1 , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       gC2H2 ,  gHCO ,  -1 ,  -1 , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       gNXC3H7 ,  -1 ,  -1 ,  -1 , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       gCH2D ,  gN2 ,  -1 ,  -1 , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       gH2O ,  mM1  ,  -1 ,  -1 , & ! Reaction # H+OH+M<=>H2O+M #
       gCO ,  gH ,  mM6  ,  -1 , & ! Reaction # HCO+M<=>CO+H+M #
       gOH ,  gH ,  mM1  ,  -1 , & ! Reaction # REV - H+OH+M<=>H2O+M #
       gHCO ,  mM6  ,  -1 ,  -1 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       gH2O2 ,  mM2  ,  -1 ,  -1 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       gHO2 ,  mM3  ,  -1 ,  -1 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       gC2H6 ,  mM4  ,  -1 ,  -1 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       gCH4 ,  mM5  ,  -1 ,  -1 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       gCH2O ,  gH ,  mM7  ,  -1 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       gC3H6 ,  mM8  ,  -1 ,  -1 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       gH ,  gC2H2 ,  mM9  ,  -1 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       gC2H5 ,  mM10  ,  -1 ,  -1 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       gC4H81 ,  mM11  ,  -1 ,  -1 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       gC3H6 ,  mM12  ,  -1 ,  -1 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       gH ,  gC4H81 ,  mM13  ,  -1 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       gC2H4 ,  gPXC4H9 ,  mM14  ,  -1 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       gOH ,  mM2  ,  -1 ,  -1 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       gO2 ,  gH ,  mM3  ,  -1 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       gCH3 ,  mM4  ,  -1 ,  -1 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       gCH3 ,  gH ,  mM5  ,  -1 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       gC2H3 ,  gCH3 ,  mM8  ,  -1 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       gC2H3 ,  mM9  ,  -1 ,  -1 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       gC2H4 ,  gH ,  mM10  ,  -1 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       gAXC3H5 ,  gCH3 ,  mM11  ,  -1 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       gH ,  gAXC3H5 ,  mM12  ,  -1 & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /), (/nproducts_max,npR/) )

  ! Number of reactants in each reaction, including thirdbodies 
  integer, parameter, dimension(npR) :: nreactants = (/ &
       2 , & ! Reaction # O+H2<=>H+OH #
       1 , & ! Reaction # 2OH<=>O+H2O #
       2 , & ! Reaction # OH+H2<=>H2O+H #
       2 , & ! Reaction # H+O2<=>OH+O #
       2 , & ! Reaction # HO2+O=>OH+O2 #
       1 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+H=>2OH #
       1 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # H+HO2=>O2+H2 #
       2 , & ! Reaction # H2O2+OH=>HO2+H2O #
       2 , & ! Reaction # H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # CH2+O=>HCO+H #
       2 , & ! Reaction # CH2+O2=>HCO+OH #
       2 , & ! Reaction # CH2+OH=>CH2O+H #
       2 , & ! Reaction # CH2+O2=>CO2+2H #
       2 , & ! Reaction # CH2D+H2<=>CH3+H #
       2 , & ! Reaction # CH2D+O2=>H+OH+CO #
       2 , & ! Reaction # CH2D+O2=>CO+H2O #
       2 , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # CH3+HO2=>CH4+O2 #
       2 , & ! Reaction # CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # CH3+CH2=>C2H4+H #
       2 , & ! Reaction # CH3+HO2=>CH3O+OH #
       2 , & ! Reaction # CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # CH3+O2=>O+CH3O #
       2 , & ! Reaction # CH3+O2=>OH+CH2O #
       2 , & ! Reaction # CH3+O=>CH2O+H #
       2 , & ! Reaction # CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # CH4+H<=>CH3+H2 #
       2 , & ! Reaction # CH4+O=>CH3+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+HO2=>CO2+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+CH2=>CO+CH2D #
       2 , & ! Reaction # HCO+OH=>CO+H2O #
       2 , & ! Reaction # HCO+O=>CO2+H #
       2 , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # HCO+H=>CO+H2 #
       2 , & ! Reaction # HCO+O=>CO+OH #
       2 , & ! Reaction # HCO+O2=>CO+HO2 #
       2 , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       2 , & ! Reaction # CH2O+H=>HCO+H2 #
       2 , & ! Reaction # CH2O+O2=>HCO+HO2 #
       2 , & ! Reaction # CH2O+OH=>HCO+H2O #
       2 , & ! Reaction # CH3+CH2O=>CH4+HCO #
       2 , & ! Reaction # CH2O+O=>HCO+OH #
       2 , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       2 , & ! Reaction # CH2D+CO2=>CH2O+CO #
       2 , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # C2H2+O=>CH2+CO #
       2 , & ! Reaction # C2H3+H=>C2H2+H2 #
       2 , & ! Reaction # C2H3+O2=>CH2CHO+O #
       2 , & ! Reaction # C2H3+O2=>HCO+CH2O #
       2 , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       2 , & ! Reaction # C2H3+OH=>C2H2+H2O #
       2 , & ! Reaction # C2H3+O=>CH3+CO #
       2 , & ! Reaction # C2H4+OH=>C2H3+H2O #
       2 , & ! Reaction # C2H4+H=>C2H3+H2 #
       2 , & ! Reaction # C2H4+O=>C2H3+OH #
       2 , & ! Reaction # C2H4+HCO=>C2H5+CO #
       2 , & ! Reaction # C2H4+O=>CH3+HCO #
       2 , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       2 , & ! Reaction # C2H5+H=>2CH3 #
       2 , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       2 , & ! Reaction # C2H6+OH=>C2H5+H2O #
       2 , & ! Reaction # C2H6+H=>C2H5+H2 #
       2 , & ! Reaction # CH2CHO+H=>CH3+HCO #
       2 , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       1 , & ! Reaction # CH2CHO=>CH3+CO #
       2 , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       2 , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       2 , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       2 , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       2 , & ! Reaction # C3H6+H=>C2H4+CH3 #
       2 , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       2 , & ! Reaction # C3H6+O=>C2H5+HCO #
       2 , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       2 , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       1 , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       2 , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       2 , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       2 , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       1 , & ! Reaction # C4H7=>C2H3+C2H4 #
       2 , & ! Reaction # C4H81+O=>C4H7+OH #
       2 , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       2 , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       2 , & ! Reaction # C4H81+H=>C4H7+H2 #
       2 , & ! Reaction # C4H81+H=>C3H6+CH3 #
       2 , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       2 , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       2 , & ! Reaction # P-C4H9+H=>2C2H5 #
       2 , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       2 , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       2 , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       2 , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       2 , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       1 , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       2 , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       2 , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       1 , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2 , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       2 , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       2 , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       2 , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1 , & ! Reaction # P-C12H25=>S3-C12H25 #
       1 , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       2 , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       2 , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       1 , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       1 , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       1 , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       1 , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       1 , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       1 , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       2 , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       1 , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       2 , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       1 , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       2 , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       1 , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       2 , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       2 , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       1 , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>C12OOH #
       1 , & ! Reaction # C12OOH=>C12H25O2 #
       2 , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       1 , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       1 , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       1 , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       2 , & ! Reaction # CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # REV - O+H2<=>H+OH #
       2 , & ! Reaction # REV - 2OH<=>O+H2O #
       2 , & ! Reaction # REV - OH+H2<=>H2O+H #
       2 , & ! Reaction # REV - H+O2<=>OH+O #
       2 , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       2 , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       3 , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       3 , & ! Reaction # H+OH+M<=>H2O+M #
       2 , & ! Reaction # HCO+M<=>CO+H+M #
       2 , & ! Reaction # REV - H+OH+M<=>H2O+M #
       3 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       2 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       3 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       2 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       3 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       2 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       3 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       2 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       3 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       3 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       3 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       2 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       3 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       2 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       2 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       2 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       2 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       2 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       3 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       2 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       2 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       2  & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /)

  ! Number of products in each reaction, including thirdbodies 
  integer, parameter, dimension(npR) :: nproducts = (/ &
       2 , & ! Reaction # O+H2<=>H+OH #
       2 , & ! Reaction # 2OH<=>O+H2O #
       2 , & ! Reaction # OH+H2<=>H2O+H #
       2 , & ! Reaction # H+O2<=>OH+O #
       2 , & ! Reaction # HO2+O=>OH+O2 #
       2 , & ! Reaction # 2HO2=>O2+H2O2 #
       1 , & ! Reaction # HO2+H=>2OH #
       2 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # H+HO2=>O2+H2 #
       2 , & ! Reaction # H2O2+OH=>HO2+H2O #
       2 , & ! Reaction # H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # CH2+O=>HCO+H #
       2 , & ! Reaction # CH2+O2=>HCO+OH #
       2 , & ! Reaction # CH2+OH=>CH2O+H #
       2 , & ! Reaction # CH2+O2=>CO2+2H #
       2 , & ! Reaction # CH2D+H2<=>CH3+H #
       3 , & ! Reaction # CH2D+O2=>H+OH+CO #
       2 , & ! Reaction # CH2D+O2=>CO+H2O #
       2 , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # CH3+HO2=>CH4+O2 #
       2 , & ! Reaction # CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # CH3+CH2=>C2H4+H #
       2 , & ! Reaction # CH3+HO2=>CH3O+OH #
       2 , & ! Reaction # CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # CH3+O2=>O+CH3O #
       2 , & ! Reaction # CH3+O2=>OH+CH2O #
       2 , & ! Reaction # CH3+O=>CH2O+H #
       2 , & ! Reaction # CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # CH4+H<=>CH3+H2 #
       2 , & ! Reaction # CH4+O=>CH3+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+HO2=>CO2+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+CH2=>CO+CH2D #
       2 , & ! Reaction # HCO+OH=>CO+H2O #
       2 , & ! Reaction # HCO+O=>CO2+H #
       3 , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # HCO+H=>CO+H2 #
       2 , & ! Reaction # HCO+O=>CO+OH #
       2 , & ! Reaction # HCO+O2=>CO+HO2 #
       2 , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       2 , & ! Reaction # CH2O+H=>HCO+H2 #
       2 , & ! Reaction # CH2O+O2=>HCO+HO2 #
       2 , & ! Reaction # CH2O+OH=>HCO+H2O #
       2 , & ! Reaction # CH3+CH2O=>CH4+HCO #
       2 , & ! Reaction # CH2O+O=>HCO+OH #
       2 , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       2 , & ! Reaction # CH2D+CO2=>CH2O+CO #
       2 , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # C2H2+O=>CH2+CO #
       2 , & ! Reaction # C2H3+H=>C2H2+H2 #
       2 , & ! Reaction # C2H3+O2=>CH2CHO+O #
       2 , & ! Reaction # C2H3+O2=>HCO+CH2O #
       2 , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       2 , & ! Reaction # C2H3+OH=>C2H2+H2O #
       2 , & ! Reaction # C2H3+O=>CH3+CO #
       2 , & ! Reaction # C2H4+OH=>C2H3+H2O #
       2 , & ! Reaction # C2H4+H=>C2H3+H2 #
       2 , & ! Reaction # C2H4+O=>C2H3+OH #
       2 , & ! Reaction # C2H4+HCO=>C2H5+CO #
       2 , & ! Reaction # C2H4+O=>CH3+HCO #
       3 , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       1 , & ! Reaction # C2H5+H=>2CH3 #
       2 , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       2 , & ! Reaction # C2H6+OH=>C2H5+H2O #
       2 , & ! Reaction # C2H6+H=>C2H5+H2 #
       2 , & ! Reaction # CH2CHO+H=>CH3+HCO #
       3 , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       2 , & ! Reaction # CH2CHO=>CH3+CO #
       3 , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       2 , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       2 , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       2 , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       2 , & ! Reaction # C3H6+H=>C2H4+CH3 #
       2 , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       2 , & ! Reaction # C3H6+O=>C2H5+HCO #
       2 , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       2 , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       2 , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       3 , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       3 , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       3 , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       2 , & ! Reaction # C4H7=>C2H3+C2H4 #
       2 , & ! Reaction # C4H81+O=>C4H7+OH #
       2 , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       2 , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       2 , & ! Reaction # C4H81+H=>C4H7+H2 #
       2 , & ! Reaction # C4H81+H=>C3H6+CH3 #
       2 , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       2 , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       1 , & ! Reaction # P-C4H9+H=>2C2H5 #
       3 , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       2 , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       2 , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       2 , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       2 , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       2 , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       2 , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       2 , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       2 , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2 , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       2 , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       3 , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       2 , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1 , & ! Reaction # P-C12H25=>S3-C12H25 #
       2 , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       1 , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       3 , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       2 , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       2 , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       2 , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       2 , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       2 , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       3 , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       1 , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       3 , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       2 , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       2 , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       3 , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       2 , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       3 , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       2 , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       2 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       2 , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       2 , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       2 , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       2 , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>C12OOH #
       1 , & ! Reaction # C12OOH=>C12H25O2 #
       1 , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       4 , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       2 , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       2 , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       2 , & ! Reaction # CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # REV - O+H2<=>H+OH #
       1 , & ! Reaction # REV - 2OH<=>O+H2O #
       2 , & ! Reaction # REV - OH+H2<=>H2O+H #
       2 , & ! Reaction # REV - H+O2<=>OH+O #
       2 , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       2 , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       1 , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # H+OH+M<=>H2O+M #
       3 , & ! Reaction # HCO+M<=>CO+H+M #
       3 , & ! Reaction # REV - H+OH+M<=>H2O+M #
       2 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       2 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       2 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       2 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       2 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       3 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       2 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       3 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       2 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       2 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       2 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       3 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       3 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       2 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       3 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       2 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       3 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       3 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       2 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       3 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       3 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       3  & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /)

  ! Number of reactants involved in reaction rate calculation 
  integer, parameter, dimension(npR) :: nreactants_inW = (/ &
       2, & ! Reaction # O+H2<=>H+OH #
       1, & ! Reaction # 2OH<=>O+H2O #
       2, & ! Reaction # OH+H2<=>H2O+H #
       2, & ! Reaction # H+O2<=>OH+O #
       2, & ! Reaction # HO2+O=>OH+O2 #
       1, & ! Reaction # 2HO2=>O2+H2O2 #
       2, & ! Reaction # HO2+H=>2OH #
       1, & ! Reaction # 2HO2=>O2+H2O2 #
       2, & ! Reaction # HO2+OH<=>H2O+O2 #
       2, & ! Reaction # H+HO2=>O2+H2 #
       2, & ! Reaction # H2O2+OH=>HO2+H2O #
       2, & ! Reaction # H2O2+OH<=>HO2+H2O #
       2, & ! Reaction # CH2+O=>HCO+H #
       2, & ! Reaction # CH2+O2=>HCO+OH #
       2, & ! Reaction # CH2+OH=>CH2O+H #
       2, & ! Reaction # CH2+O2=>CO2+2H #
       2, & ! Reaction # CH2D+H2<=>CH3+H #
       2, & ! Reaction # CH2D+O2=>H+OH+CO #
       2, & ! Reaction # CH2D+O2=>CO+H2O #
       2, & ! Reaction # CH2D+H2O<=>CH2+H2O #
       2, & ! Reaction # CH3+HO2=>CH4+O2 #
       2, & ! Reaction # CH3+OH<=>CH2+H2O #
       2, & ! Reaction # CH3+CH2=>C2H4+H #
       2, & ! Reaction # CH3+HO2=>CH3O+OH #
       2, & ! Reaction # CH3+OH<=>CH2D+H2O #
       2, & ! Reaction # CH3+O2=>O+CH3O #
       2, & ! Reaction # CH3+O2=>OH+CH2O #
       2, & ! Reaction # CH3+O=>CH2O+H #
       2, & ! Reaction # CH4+OH<=>CH3+H2O #
       2, & ! Reaction # CH4+H<=>CH3+H2 #
       2, & ! Reaction # CH4+O=>CH3+OH #
       2, & ! Reaction # CO+OH<=>CO2+H #
       2, & ! Reaction # CO+HO2=>CO2+OH #
       2, & ! Reaction # CO+OH<=>CO2+H #
       2, & ! Reaction # CO+CH2=>CO+CH2D #
       2, & ! Reaction # HCO+OH=>CO+H2O #
       2, & ! Reaction # HCO+O=>CO2+H #
       2, & ! Reaction # HCO+H2O<=>CO+H+H2O #
       2, & ! Reaction # HCO+H=>CO+H2 #
       2, & ! Reaction # HCO+O=>CO+OH #
       2, & ! Reaction # HCO+O2=>CO+HO2 #
       2, & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       2, & ! Reaction # CH2O+H=>HCO+H2 #
       2, & ! Reaction # CH2O+O2=>HCO+HO2 #
       2, & ! Reaction # CH2O+OH=>HCO+H2O #
       2, & ! Reaction # CH3+CH2O=>CH4+HCO #
       2, & ! Reaction # CH2O+O=>HCO+OH #
       2, & ! Reaction # CH3O+O2=>CH2O+HO2 #
       2, & ! Reaction # CH2D+CO2=>CH2O+CO #
       2, & ! Reaction # C2H2+HCO<=>C2H3+CO #
       2, & ! Reaction # C2H2+O=>CH2+CO #
       2, & ! Reaction # C2H3+H=>C2H2+H2 #
       2, & ! Reaction # C2H3+O2=>CH2CHO+O #
       2, & ! Reaction # C2H3+O2=>HCO+CH2O #
       2, & ! Reaction # C2H3+O2=>C2H2+HO2 #
       2, & ! Reaction # C2H3+OH=>C2H2+H2O #
       2, & ! Reaction # C2H3+O=>CH3+CO #
       2, & ! Reaction # C2H4+OH=>C2H3+H2O #
       2, & ! Reaction # C2H4+H=>C2H3+H2 #
       2, & ! Reaction # C2H4+O=>C2H3+OH #
       2, & ! Reaction # C2H4+HCO=>C2H5+CO #
       2, & ! Reaction # C2H4+O=>CH3+HCO #
       2, & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       2, & ! Reaction # C2H5+H=>2CH3 #
       2, & ! Reaction # C2H5+O2=>C2H4+HO2 #
       2, & ! Reaction # C2H6+OH=>C2H5+H2O #
       2, & ! Reaction # C2H6+H=>C2H5+H2 #
       2, & ! Reaction # CH2CHO+H=>CH3+HCO #
       2, & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       1, & ! Reaction # CH2CHO=>CH3+CO #
       2, & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       2, & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       2, & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       2, & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       2, & ! Reaction # C3H6+H=>C2H4+CH3 #
       2, & ! Reaction # C3H6+H=>A-C3H5+H2 #
       2, & ! Reaction # C3H6+O=>C2H5+HCO #
       2, & ! Reaction # C3H6+O=>C2H3CHO+2H #
       2, & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       1, & ! Reaction # N-C3H7<=>CH3+C2H4 #
       2, & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       2, & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       2, & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       2, & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       1, & ! Reaction # C4H7=>C2H3+C2H4 #
       2, & ! Reaction # C4H81+O=>C4H7+OH #
       2, & ! Reaction # C4H81+H=>C2H4+C2H5 #
       2, & ! Reaction # C4H81+O=>N-C3H7+HCO #
       2, & ! Reaction # C4H81+H=>C4H7+H2 #
       2, & ! Reaction # C4H81+H=>C3H6+CH3 #
       2, & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       2, & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       2, & ! Reaction # P-C4H9+H=>2C2H5 #
       2, & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       2, & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       2, & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       2, & ! Reaction # C5H10+H=>C3H6+C2H5 #
       2, & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       1, & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       2, & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       2, & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       2, & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       2, & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       1, & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2, & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       2, & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       2, & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       2, & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       2, & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       2, & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1, & ! Reaction # P-C12H25=>S3-C12H25 #
       1, & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       2, & ! Reaction # P-C12H25+O2=>C12H25O2 #
       2, & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       1, & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       1, & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       1, & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       1, & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       1, & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       1, & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       1, & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       2, & ! Reaction # S-C12H25+O2=>C12H25O2 #
       1, & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       1, & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       2, & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       1, & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       2, & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       2, & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       2, & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       2, & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       1, & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       2, & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       1, & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2, & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       2, & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       2, & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       1, & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2, & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       2, & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       2, & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       2, & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       2, & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       2, & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       2, & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       2, & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       1, & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       1, & ! Reaction # C12H25O2=>S-C12H25+O2 #
       1, & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1, & ! Reaction # C12H25O2=>C12OOH #
       1, & ! Reaction # C12OOH=>C12H25O2 #
       2, & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       1, & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       1, & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       1, & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       2, & ! Reaction # CH2D+N2<=>CH2+N2 #
       2, & ! Reaction # REV - O+H2<=>H+OH #
       2, & ! Reaction # REV - 2OH<=>O+H2O #
       2, & ! Reaction # REV - OH+H2<=>H2O+H #
       2, & ! Reaction # REV - H+O2<=>OH+O #
       2, & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       2, & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       2, & ! Reaction # REV - CH2D+H2<=>CH3+H #
       2, & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       2, & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       2, & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       2, & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       2, & ! Reaction # REV - CH4+H<=>CH3+H2 #
       2, & ! Reaction # REV - CO+OH<=>CO2+H #
       2, & ! Reaction # REV - CO+OH<=>CO2+H #
       3, & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       2, & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       2, & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       2, & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       3, & ! Reaction # H+OH+M<=>H2O+M #
       2, & ! Reaction # HCO+M<=>CO+H+M #
       2, & ! Reaction # REV - H+OH+M<=>H2O+M #
       3, & ! Reaction # REV - HCO+M<=>CO+H+M #
       1 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       2 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       2 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       1 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       2 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       1 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       2 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       2 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       2 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       1 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       2 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       1 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       1 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       1 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       1 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       2 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       1 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       1 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       1  & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /)

  ! Number of reactants without counting thirdbodies 
  integer, parameter, dimension(npR) :: nreactants_noM = (/ &
       2 , & ! Reaction # O+H2<=>H+OH #
       1 , & ! Reaction # 2OH<=>O+H2O #
       2 , & ! Reaction # OH+H2<=>H2O+H #
       2 , & ! Reaction # H+O2<=>OH+O #
       2 , & ! Reaction # HO2+O=>OH+O2 #
       1 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+H=>2OH #
       1 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # H+HO2=>O2+H2 #
       2 , & ! Reaction # H2O2+OH=>HO2+H2O #
       2 , & ! Reaction # H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # CH2+O=>HCO+H #
       2 , & ! Reaction # CH2+O2=>HCO+OH #
       2 , & ! Reaction # CH2+OH=>CH2O+H #
       2 , & ! Reaction # CH2+O2=>CO2+2H #
       2 , & ! Reaction # CH2D+H2<=>CH3+H #
       2 , & ! Reaction # CH2D+O2=>H+OH+CO #
       2 , & ! Reaction # CH2D+O2=>CO+H2O #
       2 , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # CH3+HO2=>CH4+O2 #
       2 , & ! Reaction # CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # CH3+CH2=>C2H4+H #
       2 , & ! Reaction # CH3+HO2=>CH3O+OH #
       2 , & ! Reaction # CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # CH3+O2=>O+CH3O #
       2 , & ! Reaction # CH3+O2=>OH+CH2O #
       2 , & ! Reaction # CH3+O=>CH2O+H #
       2 , & ! Reaction # CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # CH4+H<=>CH3+H2 #
       2 , & ! Reaction # CH4+O=>CH3+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+HO2=>CO2+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+CH2=>CO+CH2D #
       2 , & ! Reaction # HCO+OH=>CO+H2O #
       2 , & ! Reaction # HCO+O=>CO2+H #
       2 , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # HCO+H=>CO+H2 #
       2 , & ! Reaction # HCO+O=>CO+OH #
       2 , & ! Reaction # HCO+O2=>CO+HO2 #
       2 , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       2 , & ! Reaction # CH2O+H=>HCO+H2 #
       2 , & ! Reaction # CH2O+O2=>HCO+HO2 #
       2 , & ! Reaction # CH2O+OH=>HCO+H2O #
       2 , & ! Reaction # CH3+CH2O=>CH4+HCO #
       2 , & ! Reaction # CH2O+O=>HCO+OH #
       2 , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       2 , & ! Reaction # CH2D+CO2=>CH2O+CO #
       2 , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # C2H2+O=>CH2+CO #
       2 , & ! Reaction # C2H3+H=>C2H2+H2 #
       2 , & ! Reaction # C2H3+O2=>CH2CHO+O #
       2 , & ! Reaction # C2H3+O2=>HCO+CH2O #
       2 , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       2 , & ! Reaction # C2H3+OH=>C2H2+H2O #
       2 , & ! Reaction # C2H3+O=>CH3+CO #
       2 , & ! Reaction # C2H4+OH=>C2H3+H2O #
       2 , & ! Reaction # C2H4+H=>C2H3+H2 #
       2 , & ! Reaction # C2H4+O=>C2H3+OH #
       2 , & ! Reaction # C2H4+HCO=>C2H5+CO #
       2 , & ! Reaction # C2H4+O=>CH3+HCO #
       2 , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       2 , & ! Reaction # C2H5+H=>2CH3 #
       2 , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       2 , & ! Reaction # C2H6+OH=>C2H5+H2O #
       2 , & ! Reaction # C2H6+H=>C2H5+H2 #
       2 , & ! Reaction # CH2CHO+H=>CH3+HCO #
       2 , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       1 , & ! Reaction # CH2CHO=>CH3+CO #
       2 , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       2 , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       2 , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       2 , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       2 , & ! Reaction # C3H6+H=>C2H4+CH3 #
       2 , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       2 , & ! Reaction # C3H6+O=>C2H5+HCO #
       2 , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       2 , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       1 , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       2 , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       2 , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       2 , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       1 , & ! Reaction # C4H7=>C2H3+C2H4 #
       2 , & ! Reaction # C4H81+O=>C4H7+OH #
       2 , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       2 , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       2 , & ! Reaction # C4H81+H=>C4H7+H2 #
       2 , & ! Reaction # C4H81+H=>C3H6+CH3 #
       2 , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       2 , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       2 , & ! Reaction # P-C4H9+H=>2C2H5 #
       2 , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       2 , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       2 , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       2 , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       2 , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       1 , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       2 , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       2 , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       1 , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2 , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       2 , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       2 , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       2 , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1 , & ! Reaction # P-C12H25=>S3-C12H25 #
       1 , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       2 , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       2 , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       1 , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       1 , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       1 , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       1 , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       1 , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       1 , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       2 , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       1 , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       2 , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       1 , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       2 , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       1 , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       2 , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       1 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       2 , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       1 , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>C12OOH #
       1 , & ! Reaction # C12OOH=>C12H25O2 #
       2 , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       1 , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       1 , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       1 , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       2 , & ! Reaction # CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # REV - O+H2<=>H+OH #
       2 , & ! Reaction # REV - 2OH<=>O+H2O #
       2 , & ! Reaction # REV - OH+H2<=>H2O+H #
       2 , & ! Reaction # REV - H+O2<=>OH+O #
       2 , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       2 , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       3 , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # H+OH+M<=>H2O+M #
       1 , & ! Reaction # HCO+M<=>CO+H+M #
       1 , & ! Reaction # REV - H+OH+M<=>H2O+M #
       2 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       1 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       2 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       2 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       1 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       2 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       1 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       2 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       2 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       2 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       1 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       2 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       1 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       1 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       1 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       1 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       2 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       1 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       1 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       1 & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /)

  ! Number of products without counting thirdbodies 
  integer, parameter, dimension(npR) :: nproducts_noM = (/ &
       2 , & ! Reaction # O+H2<=>H+OH #
       2 , & ! Reaction # 2OH<=>O+H2O #
       2 , & ! Reaction # OH+H2<=>H2O+H #
       2 , & ! Reaction # H+O2<=>OH+O #
       2 , & ! Reaction # HO2+O=>OH+O2 #
       2 , & ! Reaction # 2HO2=>O2+H2O2 #
       1 , & ! Reaction # HO2+H=>2OH #
       2 , & ! Reaction # 2HO2=>O2+H2O2 #
       2 , & ! Reaction # HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # H+HO2=>O2+H2 #
       2 , & ! Reaction # H2O2+OH=>HO2+H2O #
       2 , & ! Reaction # H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # CH2+O=>HCO+H #
       2 , & ! Reaction # CH2+O2=>HCO+OH #
       2 , & ! Reaction # CH2+OH=>CH2O+H #
       2 , & ! Reaction # CH2+O2=>CO2+2H #
       2 , & ! Reaction # CH2D+H2<=>CH3+H #
       3 , & ! Reaction # CH2D+O2=>H+OH+CO #
       2 , & ! Reaction # CH2D+O2=>CO+H2O #
       2 , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # CH3+HO2=>CH4+O2 #
       2 , & ! Reaction # CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # CH3+CH2=>C2H4+H #
       2 , & ! Reaction # CH3+HO2=>CH3O+OH #
       2 , & ! Reaction # CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # CH3+O2=>O+CH3O #
       2 , & ! Reaction # CH3+O2=>OH+CH2O #
       2 , & ! Reaction # CH3+O=>CH2O+H #
       2 , & ! Reaction # CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # CH4+H<=>CH3+H2 #
       2 , & ! Reaction # CH4+O=>CH3+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+HO2=>CO2+OH #
       2 , & ! Reaction # CO+OH<=>CO2+H #
       2 , & ! Reaction # CO+CH2=>CO+CH2D #
       2 , & ! Reaction # HCO+OH=>CO+H2O #
       2 , & ! Reaction # HCO+O=>CO2+H #
       3 , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # HCO+H=>CO+H2 #
       2 , & ! Reaction # HCO+O=>CO+OH #
       2 , & ! Reaction # HCO+O2=>CO+HO2 #
       2 , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       2 , & ! Reaction # CH2O+H=>HCO+H2 #
       2 , & ! Reaction # CH2O+O2=>HCO+HO2 #
       2 , & ! Reaction # CH2O+OH=>HCO+H2O #
       2 , & ! Reaction # CH3+CH2O=>CH4+HCO #
       2 , & ! Reaction # CH2O+O=>HCO+OH #
       2 , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       2 , & ! Reaction # CH2D+CO2=>CH2O+CO #
       2 , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       2 , & ! Reaction # C2H2+O=>CH2+CO #
       2 , & ! Reaction # C2H3+H=>C2H2+H2 #
       2 , & ! Reaction # C2H3+O2=>CH2CHO+O #
       2 , & ! Reaction # C2H3+O2=>HCO+CH2O #
       2 , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       2 , & ! Reaction # C2H3+OH=>C2H2+H2O #
       2 , & ! Reaction # C2H3+O=>CH3+CO #
       2 , & ! Reaction # C2H4+OH=>C2H3+H2O #
       2 , & ! Reaction # C2H4+H=>C2H3+H2 #
       2 , & ! Reaction # C2H4+O=>C2H3+OH #
       2 , & ! Reaction # C2H4+HCO=>C2H5+CO #
       2 , & ! Reaction # C2H4+O=>CH3+HCO #
       3 , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       1 , & ! Reaction # C2H5+H=>2CH3 #
       2 , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       2 , & ! Reaction # C2H6+OH=>C2H5+H2O #
       2 , & ! Reaction # C2H6+H=>C2H5+H2 #
       2 , & ! Reaction # CH2CHO+H=>CH3+HCO #
       3 , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       2 , & ! Reaction # CH2CHO=>CH3+CO #
       3 , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       2 , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       2 , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       2 , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       2 , & ! Reaction # C3H6+H=>C2H4+CH3 #
       2 , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       2 , & ! Reaction # C3H6+O=>C2H5+HCO #
       2 , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       2 , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       2 , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       3 , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       3 , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       3 , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       2 , & ! Reaction # C4H7=>C2H3+C2H4 #
       2 , & ! Reaction # C4H81+O=>C4H7+OH #
       2 , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       2 , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       2 , & ! Reaction # C4H81+H=>C4H7+H2 #
       2 , & ! Reaction # C4H81+H=>C3H6+CH3 #
       2 , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       2 , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       1 , & ! Reaction # P-C4H9+H=>2C2H5 #
       3 , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       2 , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       2 , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       2 , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       2 , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       2 , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       2 , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       2 , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       2 , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       2 , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2 , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       2 , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       3 , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       2 , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       2 , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1 , & ! Reaction # P-C12H25=>S3-C12H25 #
       2 , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       1 , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       1 , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       3 , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       2 , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       2 , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       2 , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       2 , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       2 , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       3 , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       1 , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       3 , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       2 , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       2 , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       3 , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       2 , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       3 , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       2 , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       2 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       2 , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       2 , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       2 , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       2 , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       2 , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       2 , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       2 , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       2 , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       2 , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       2 , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1 , & ! Reaction # C12H25O2=>C12OOH #
       1 , & ! Reaction # C12OOH=>C12H25O2 #
       1 , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       4 , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       2 , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       2 , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       2 , & ! Reaction # CH2D+N2<=>CH2+N2 #
       2 , & ! Reaction # REV - O+H2<=>H+OH #
       1 , & ! Reaction # REV - 2OH<=>O+H2O #
       2 , & ! Reaction # REV - OH+H2<=>H2O+H #
       2 , & ! Reaction # REV - H+O2<=>OH+O #
       2 , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       2 , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       2 , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       2 , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       2 , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       2 , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       2 , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - CO+OH<=>CO2+H #
       2 , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       2 , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       1 , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       2 , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       1 , & ! Reaction # H+OH+M<=>H2O+M #
       2 , & ! Reaction # HCO+M<=>CO+H+M #
       2 , & ! Reaction # REV - H+OH+M<=>H2O+M #
       1 , & ! Reaction # REV - HCO+M<=>CO+H+M #
       1 , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       1 , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       1 , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       2 , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       1 , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       2 , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       1 , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       1 , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       1 , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       2 , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       2 , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       1 , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       2 , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       1 , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       2 , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       2 , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       1 , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       2 , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       2 , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       2 & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /)

  ! Stoichiometric coefficients of reactants 
  real(WP), parameter, dimension(nreactants_max,npR) :: nuR = reshape( (/ &
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # O+H2<=>H+OH #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2OH<=>O+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # OH+H2<=>H2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # H+O2<=>OH+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HO2+O=>OH+O2 #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2HO2=>O2+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HO2+H=>2OH #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2HO2=>O2+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HO2+OH<=>H2O+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # H+HO2=>O2+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # H2O2+OH=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # H2O2+OH<=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2+O=>HCO+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2+O2=>HCO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2+OH=>CH2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2+O2=>CO2+2H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+H2<=>CH3+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+O2=>H+OH+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+O2=>CO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+HO2=>CH4+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+OH<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+CH2=>C2H4+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+HO2=>CH3O+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+OH<=>CH2D+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+O2=>O+CH3O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+O2=>OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+O=>CH2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH4+OH<=>CH3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH4+H<=>CH3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH4+O=>CH3+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CO+HO2=>CO2+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CO+CH2=>CO+CH2D #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+OH=>CO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+O=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+H=>CO+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+O=>CO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+O2=>CO+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2O+H=>HCO+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2O+O2=>HCO+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2O+OH=>HCO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3+CH2O=>CH4+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2O+O=>HCO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+CO2=>CH2O+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H2+O=>CH2+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+H=>C2H2+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>CH2CHO+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>HCO+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+OH=>C2H2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3+O=>CH3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H4+OH=>C2H3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H4+H=>C2H3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H4+O=>C2H3+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H4+HCO=>C2H5+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H4+O=>CH3+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H5+H=>2CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H6+OH=>C2H5+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H6+H=>C2H5+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2CHO+H=>CH3+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2CHO=>CH3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C3H6+H=>C2H4+CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C3H6+O=>C2H5+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H7=>C2H3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H81+O=>C4H7+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C4H7+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C3H6+CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+H=>2C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C12H25=>S3-C12H25 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>C12OOH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12OOH=>C12H25O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+N2<=>CH2+N2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - O+H2<=>H+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - 2OH<=>O+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - OH+H2<=>H2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H+O2<=>OH+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # H+OH+M<=>H2O+M #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+M<=>CO+H+M #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H+OH+M<=>H2O+M #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # REV - HCO+M<=>CO+H+M #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /), (/nreactants_max,npR/) )

  ! Stoichiometric coefficients of products 
  real(WP), parameter, dimension(nproducts_max,npR) :: nuP = reshape( (/ &
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # O+H2<=>H+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2OH<=>O+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # OH+H2<=>H2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H+O2<=>OH+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HO2+O=>OH+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2HO2=>O2+H2O2 #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HO2+H=>2OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2HO2=>O2+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HO2+OH<=>H2O+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H+HO2=>O2+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H2O2+OH=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H2O2+OH<=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2+O=>HCO+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2+O2=>HCO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2+OH=>CH2O+H #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2+O2=>CO2+2H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2D+H2<=>CH3+H #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2D+O2=>H+OH+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2D+O2=>CO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2D+H2O<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+HO2=>CH4+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+OH<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+CH2=>C2H4+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+HO2=>CH3O+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+OH<=>CH2D+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+O2=>O+CH3O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+O2=>OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+O=>CH2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH4+OH<=>CH3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH4+H<=>CH3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH4+O=>CH3+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CO+HO2=>CO2+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CO+CH2=>CO+CH2D #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HCO+OH=>CO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HCO+O=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+H2O<=>CO+H+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HCO+H=>CO+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HCO+O=>CO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # HCO+O2=>CO+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2O+HO2=>HCO+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2O+H=>HCO+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2O+O2=>HCO+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2O+OH=>HCO+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+CH2O=>CH4+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2O+O=>HCO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3O+O2=>CH2O+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2D+CO2=>CH2O+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H2+HCO<=>C2H3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H2+O=>CH2+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+H=>C2H2+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>CH2CHO+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>HCO+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+O2=>C2H2+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+OH=>C2H2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+O=>CH3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+OH=>C2H3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+H=>C2H3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+O=>C2H3+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+HCO=>C2H5+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+O=>CH3+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H5+HO2=>CH3+CH2O+OH #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H5+H=>2CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H5+O2=>C2H4+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H6+OH=>C2H5+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H6+H=>C2H5+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2CHO+H=>CH3+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH2CHO+O2=>CH2O+CO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2CHO=>CH3+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # A-C3H5+HO2=>OH+C2H3+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # A-C3H5+HO2=>C3H6+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H+A-C3H5=>CH3+C2H3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # A-C3H5+O=>C2H3CHO+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C3H6+H=>C2H4+CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C3H6+H=>A-C3H5+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C3H6+O=>C2H5+HCO #
       1.000000000000_WP ,  2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C3H6+O=>C2H3CHO+2H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C3H6+OH=>A-C3H5+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C3H7<=>CH3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C3H7+O2=>C3H6+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C3H7+HO2=>C2H5+OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3CHO+OH=>C2H3+H2O+CO #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C4H7+HO2=>CH2O+OH+A-C3H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H7=>C2H3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H81+O=>C4H7+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C2H4+C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H81+O=>N-C3H7+HCO #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C4H7+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C4H81+H=>C3H6+CH3 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C4H9+CH3=>C4H81+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C4H9+O=>N-C3H7+CH2O #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C4H9+H=>2C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9+HO2=>N-C3H7+OH+CH2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C4H9+OH=>C4H81+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C4H9+O2=>C4H81+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C5H10+H=>C3H6+C2H5 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C5H10+H=>C2H4+N-C3H7 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C5H11=>N-C3H7+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C6H12+H=>C3H6+N-C3H7 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C6H12+H=>C2H4+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C7H14+H=>C3H6+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C7H14+H=>C2H4+P-C5H11 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C7H15=>P-C5H11+C2H4 #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C8H16+H=>2C2H4+P-C4H9 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C8H16+H=>C3H6+P-C5H11 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C9H18+H=>C3H6+P-C4H9+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C9H18+H=>C2H4+P-C7H15 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C10H20+H=>C3H6+P-C7H15 #
       1.000000000000_WP ,  3.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C10H20+H=>3C2H4+P-C4H9 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C12H25=>S3-C12H25 #
       4.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C12H25=>P-C4H9+4C2H4 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # P-C12H25+O2=>C12H25O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25+O2=>C12H25O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C4H9+C2H4+C6H12 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>C2H5+C10H20 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>N-C3H7+C9H18 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C4H9+C8H16 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C7H15+C5H10 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S3-C12H25=>P-C5H11+C7H14 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # S-C12H25=>P-C7H15+C2H4+C3H6 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # S-C12H25+O2=>C12H25O2 #
       2.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # S-C12H25=>P-C4H9+2C2H4+C4H81 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>P-C5H11+P-C7H15 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>P-C12H25+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26=>N-C3H7+P-C7H15+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>P-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>S-C12H25+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>S3-C12H25+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>P-C12H25+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  3.000000000000_WP ,  0.0_WP , & ! Reaction # N-C12H26=>C2H5+P-C4H9+3C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>P-C12H25+OH #
       2.000000000000_WP ,  2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>S-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>S3-C12H25+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+O2=>S3-C12H25+HO2 #
       2.000000000000_WP ,  2.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26=>2P-C4H9+2C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>P-C12H25+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>S-C12H25+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+H=>S3-C12H25+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+HO2=>S-C12H25+H2O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+CH3=>S3-C12H25+CH4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+O2=>S-C12H25+HO2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+O=>S3-C12H25+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # N-C12H26+OH=>S-C12H25+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>S3-C12H25+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>S-C12H25+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>P-C12H25+O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12H25O2=>C12OOH #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12OOH=>C12H25O2 #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C12OOH+O2=>O2C12H24OOH #
       1.000000000000_WP ,  2.000000000000_WP ,  3.000000000000_WP ,  1.000000000000_WP , & ! Reaction # OC12H23OOH=>3C2H4+C2H5+2CH2CHO+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # O2C12H24OOH=>OC12H23OOH+OH #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # O2C12H24OOH=>C12OOH+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH2D+N2<=>CH2+N2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - O+H2<=>H+OH #
       2.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - 2OH<=>O+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - OH+H2<=>H2O+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - H+O2<=>OH+O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - HO2+OH<=>H2O+O2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - H2O2+OH<=>HO2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH2D+H2<=>CH3+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH2D+H2O<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH3+OH<=>CH2+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH3+OH<=>CH2D+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH4+OH<=>CH3+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH4+H<=>CH3+H2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CO+OH<=>CO2+H #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - HCO+H2O<=>CO+H+H2O #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - C2H2+HCO<=>C2H3+CO #
       1.000000000000_WP ,  0.0_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - N-C3H7<=>CH3+C2H4 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - CH2D+N2<=>CH2+N2 #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H+OH+M<=>H2O+M #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # HCO+M<=>CO+H+M #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H+OH+M<=>H2O+M #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - HCO+M<=>CO+H+M #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2OH(+M)<=>H2O2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # H+O2(+M)<=>HO2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # 2CH3(+M)<=>C2H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # CH3+H(+M)<=>CH4(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # CH3O(+M)=>H+CH2O(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H3+CH3(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C2H3(+M)<=>C2H2+H(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # C2H4+H(+M)<=>C2H5(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # A-C3H5+CH3(+M)<=>C4H81(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # A-C3H5+H(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # P-C4H9(+M)=>H+C4H81(+M) #
       2.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # C8H16+H(+M)=>P-C4H9+2C2H4(+M) #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - 2OH(+M)<=>H2O2(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - H+O2(+M)<=>HO2(+M) #
       2.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - 2CH3(+M)<=>C2H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - CH3+H(+M)<=>CH4(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - C2H3+CH3(+M)<=>C3H6(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP ,  0.0_WP , & ! Reaction # REV - C2H3(+M)<=>C2H2+H(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - C2H4+H(+M)<=>C2H5(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP , & ! Reaction # REV - A-C3H5+CH3(+M)<=>C4H81(+M) #
       1.000000000000_WP ,  1.000000000000_WP ,  1.000000000000_WP ,  0.0_WP & ! Reaction # REV - A-C3H5+H(+M)<=>C3H6(+M) #
  /), (/nproducts_max,npR/) )

  ! k over epsilon from transport file 
  real(WP), parameter, dimension(npS) :: koveps = (/ &
       0.0102532554085922_WP , & ! koveps(gN2)
       0.0125_WP , & ! koveps(gO)
       0.0263157894736842_WP , & ! koveps(gH2)
       0.00689655172413793_WP , & ! koveps(gH)
       0.0125_WP , & ! koveps(gOH)
       0.00174703004891684_WP , & ! koveps(gH2O)
       0.00931098696461825_WP , & ! koveps(gH2O2)
       0.00931098696461825_WP , & ! koveps(gO2)
       0.00931098696461825_WP , & ! koveps(gHO2)
       0.00200803212851406_WP , & ! koveps(gCH2O)
       0.00409836065573771_WP , & ! koveps(gCO2)
       0.00694444444444444_WP , & ! koveps(gCH3)
       0.0101936799184506_WP , & ! koveps(gCO)
       0.00396353547364249_WP , & ! koveps(gC2H6)
       0.00707213578500707_WP , & ! koveps(gCH4)
       0.00356125356125356_WP , & ! koveps(gC2H4)
       0.00478468899521531_WP , & ! koveps(gC2H2)
       0.00374812593703148_WP , & ! koveps(gC3H6)
       0.00280112044817927_WP , & ! koveps(gC4H81)
       0.00222961463340676_WP , & ! koveps(gC5H10)
       0.00198165384866902_WP , & ! koveps(gC6H12)
       0.00179228493028908_WP , & ! koveps(gC7H14)
       0.00164341778309515_WP , & ! koveps(gC8H16)
       0.0015258090602542_WP , & ! koveps(gC9H18)
       0.00143241439175388_WP , & ! koveps(gC10H20)
       0.00124218818902626_WP , & ! koveps(gC12H25O2)
       0.00126585483176789_WP , & ! koveps(gNXC12H26)
       0.00119140094454267_WP , & ! koveps(gOC12H23OOH)
       0.00694444444444444_WP , & ! koveps(gCH2)
       0.00200803212851406_WP , & ! koveps(gHCO)
       0.00694444444444444_WP , & ! koveps(gCH2D)
       0.00239808153477218_WP , & ! koveps(gCH3O)
       0.00478468899521531_WP , & ! koveps(gC2H3)
       0.00229357798165138_WP , & ! koveps(gCH2CHO)
       0.00396353547364249_WP , & ! koveps(gC2H5)
       0.00374812593703148_WP , & ! koveps(gAXC3H5)
       0.00280112044817927_WP , & ! koveps(gC2H3CHO)
       0.00374812593703148_WP , & ! koveps(gNXC3H7)
       0.00280112044817927_WP , & ! koveps(gC4H7)
       0.00280112044817927_WP , & ! koveps(gPXC4H9)
       0.00218253881645285_WP , & ! koveps(gPXC5H11)
       0.001772955339255_WP , & ! koveps(gPXC7H15)
       0.00126585483176789_WP , & ! koveps(gPXC12H25)
       0.00126585483176789_WP , & ! koveps(gS3XC12H25)
       0.00126585483176789_WP , & ! koveps(gSXC12H25)
       0.00124218818902626_WP , & ! koveps(gC12OOH)
       0.00114018325025198_WP & ! koveps(gO2C12H24OOH)
  /)

  ! Constant part of viscosity 
  real(WP), parameter, dimension(npS) :: mucoeff =(/ &
       1.07775110505506e-06_WP , & ! mucoeff(gN2)
       1.41200444750241e-06_WP , & ! mucoeff(gO)
       4.44550417011021e-07_WP , & ! mucoeff(gH2)
       6.37769879982803e-07_WP , & ! mucoeff(gH)
       1.45580329132472e-06_WP , & ! mucoeff(gOH)
       1.66976438001032e-06_WP , & ! mucoeff(gH2O)
       1.30206631757929e-06_WP , & ! mucoeff(gH2O2)
       1.26289275640534e-06_WP , & ! mucoeff(gO2)
       1.28262909867888e-06_WP , & ! mucoeff(gHO2)
       1.13501422351609e-06_WP , & ! mucoeff(gCH2O)
       1.25068721004271e-06_WP , & ! mucoeff(gCO2)
       7.16822354707612e-07_WP , & ! mucoeff(gCH3)
       1.06050394031311e-06_WP , & ! mucoeff(gCO)
       7.90957083302567e-07_WP , & ! mucoeff(gC2H6)
       7.61965259548099e-07_WP , & ! mucoeff(gCH4)
       8.9665134024098e-07_WP , & ! mucoeff(gC2H4)
       8.10328062019567e-07_WP , & ! mucoeff(gC2H2)
       6.97688491422334e-07_WP , & ! mucoeff(gC3H6)
       7.46362547902439e-07_WP , & ! mucoeff(gC4H81)
       7.83403690403724e-07_WP , & ! mucoeff(gC5H10)
       7.73171480199247e-07_WP , & ! mucoeff(gC6H12)
       7.66114750258703e-07_WP , & ! mucoeff(gC7H14)
       7.5996519305009e-07_WP , & ! mucoeff(gC8H16)
       7.4831595661231e-07_WP , & ! mucoeff(gC9H18)
       7.30668929322271e-07_WP , & ! mucoeff(gC10H20)
       7.50971725549508e-07_WP , & ! mucoeff(gC12H25O2)
       7.01577966788944e-07_WP , & ! mucoeff(gNXC12H26)
       7.42257496055466e-07_WP , & ! mucoeff(gOC12H23OOH)
       6.92374692110565e-07_WP , & ! mucoeff(gCH2)
       1.11579985852715e-06_WP , & ! mucoeff(gHCO)
       6.92374692110565e-07_WP , & ! mucoeff(gCH2D)
       1.09221366687395e-06_WP , & ! mucoeff(gCH3O)
       8.2586528522194e-07_WP , & ! mucoeff(gC2H3)
       1.11126275501992e-06_WP , & ! mucoeff(gCH2CHO)
       7.77586037779486e-07_WP , & ! mucoeff(gC2H5)
       6.89281093096124e-07_WP , & ! mucoeff(gAXC3H5)
       7.4608312846783e-07_WP , & ! mucoeff(gC2H3CHO)
       7.05995776776714e-07_WP , & ! mucoeff(gNXC3H7)
       7.39627347426793e-07_WP , & ! mucoeff(gC4H7)
       7.53037510859352e-07_WP , & ! mucoeff(gPXC4H9)
       7.59445334328542e-07_WP , & ! mucoeff(gPXC5H11)
       7.37554367330438e-07_WP , & ! mucoeff(gPXC7H15)
       6.99498919347197e-07_WP , & ! mucoeff(gPXC12H25)
       6.99498919347197e-07_WP , & ! mucoeff(gS3XC12H25)
       6.99498919347197e-07_WP , & ! mucoeff(gSXC12H25)
       7.50971725549508e-07_WP , & ! mucoeff(gC12OOH)
       7.33312619317359e-07_WP  & ! mucoeff(gO2C12H24OOH)
  /)

  ! Constant part of binary diffusion coefficients 
  real(WP), parameter, dimension(npS,npS) :: Dcoeffs = reshape ( (/ &
       0.000383765760376417_WP , & ! Dcoeffs(gN2,gN2)
       0.000581591546093125_WP , & ! Dcoeffs(gO,gN2)
       0.00128397259158685_WP , & ! Dcoeffs(gH2,gN2)
       0.00237480122367031_WP , & ! Dcoeffs(gH,gN2)
       0.000570515913021803_WP , & ! Dcoeffs(gOH,gN2)
       0.000586907472353413_WP , & ! Dcoeffs(gH2O,gN2)
       0.000383534746377616_WP , & ! Dcoeffs(gH2O2,gN2)
       0.000388953289838259_WP , & ! Dcoeffs(gO2,gN2)
       0.000386170777248283_WP , & ! Dcoeffs(gHO2,gN2)
       0.000380552592543488_WP , & ! Dcoeffs(gCH2O,gN2)
       0.000333938055929546_WP , & ! Dcoeffs(gCO2,gN2)
       0.000437333639705725_WP , & ! Dcoeffs(gCH3,gN2)
       0.00038074458458987_WP , & ! Dcoeffs(gCO,gN2)
       0.000315122850778215_WP , & ! Dcoeffs(gC2H6,gN2)
       0.000434600052868636_WP , & ! Dcoeffs(gCH4,gN2)
       0.000349097691543822_WP , & ! Dcoeffs(gC2H4,gN2)
       0.000343998052653477_WP , & ! Dcoeffs(gC2H2,gN2)
       0.000248195378531972_WP , & ! Dcoeffs(gC3H6,gN2)
       0.000225196754854586_WP , & ! Dcoeffs(gC4H81,gN2)
       0.000209582350691676_WP , & ! Dcoeffs(gC5H10,gN2)
       0.0001920817664437_WP , & ! Dcoeffs(gC6H12,gN2)
       0.000178900919575114_WP , & ! Dcoeffs(gC7H14,gN2)
       0.000168365009971315_WP , & ! Dcoeffs(gC8H16,gN2)
       0.00015884647366511_WP , & ! Dcoeffs(gC9H18,gN2)
       0.000149865759257987_WP , & ! Dcoeffs(gC10H20,gN2)
       0.000132108906772413_WP , & ! Dcoeffs(gC12H25O2,gN2)
       0.000134950078900224_WP , & ! Dcoeffs(gNXC12H26,gN2)
       0.000127451043084698_WP , & ! Dcoeffs(gOC12H23OOH,gN2)
       0.000447444145472319_WP , & ! Dcoeffs(gCH2,gN2)
       0.000383729937200005_WP , & ! Dcoeffs(gHCO,gN2)
       0.000447444145472319_WP , & ! Dcoeffs(gCH2D,gN2)
       0.000367299622176243_WP , & ! Dcoeffs(gCH3O,gN2)
       0.000340658772501297_WP , & ! Dcoeffs(gC2H3,gN2)
       0.000317350215668815_WP , & ! Dcoeffs(gCH2CHO,gN2)
       0.000317748224750734_WP , & ! Dcoeffs(gC2H5,gN2)
       0.00024940988755713_WP , & ! Dcoeffs(gAXC3H5,gN2)
       0.000225224850173203_WP , & ! Dcoeffs(gC2H3CHO,gN2)
       0.000247032138144801_WP , & ! Dcoeffs(gNXC3H7,gN2)
       0.000225881865460659_WP , & ! Dcoeffs(gC4H7,gN2)
       0.00022453384695368_WP , & ! Dcoeffs(gPXC4H9,gN2)
       0.000204432477443442_WP , & ! Dcoeffs(gPXC5H11,gN2)
       0.000173977654787646_WP , & ! Dcoeffs(gPXC7H15,gN2)
       0.00013500681303601_WP , & ! Dcoeffs(gPXC12H25,gN2)
       0.00013500681303601_WP , & ! Dcoeffs(gS3XC12H25,gN2)
       0.00013500681303601_WP , & ! Dcoeffs(gSXC12H25,gN2)
       0.000132108906772413_WP , & ! Dcoeffs(gC12OOH,gN2)
       0.000122736152591483_WP , & ! Dcoeffs(gO2C12H24OOH,gN2)
       0.000581591546093125_WP , & ! Dcoeffs(gN2,gO)
       0.000880505271614372_WP , & ! Dcoeffs(gO,gO)
       0.00175129838232579_WP , & ! Dcoeffs(gH2,gO)
       0.00335781082129878_WP , & ! Dcoeffs(gH,gO)
       0.000867361107237895_WP , & ! Dcoeffs(gOH,gO)
       0.000902476342426892_WP , & ! Dcoeffs(gH2O,gO)
       0.00059258653884259_WP , & ! Dcoeffs(gH2O2,gO)
       0.000598528113688703_WP , & ! Dcoeffs(gO2,gO)
       0.00059547400789023_WP , & ! Dcoeffs(gHO2,gO)
       0.000580118482525006_WP , & ! Dcoeffs(gCH2O,gO)
       0.000518461264040031_WP , & ! Dcoeffs(gCO2,gO)
       0.000630726889631031_WP , & ! Dcoeffs(gCH3,gO)
       0.00057637020671351_WP , & ! Dcoeffs(gCO,gO)
       0.000468775755742338_WP , & ! Dcoeffs(gC2H6,gO)
       0.000630784279408771_WP , & ! Dcoeffs(gCH4,gO)
       0.000522486953956379_WP , & ! Dcoeffs(gC2H4,gO)
       0.000510017125100313_WP , & ! Dcoeffs(gC2H2,gO)
       0.00037011520958668_WP , & ! Dcoeffs(gC3H6,gO)
       0.000339873240031536_WP , & ! Dcoeffs(gC4H81,gO)
       0.000318754186562301_WP , & ! Dcoeffs(gC5H10,gO)
       0.00029272348894414_WP , & ! Dcoeffs(gC6H12,gO)
       0.000272964594538869_WP , & ! Dcoeffs(gC7H14,gO)
       0.000257040437728607_WP , & ! Dcoeffs(gC8H16,gO)
       0.000242431210566166_WP , & ! Dcoeffs(gC9H18,gO)
       0.000228466358448342_WP , & ! Dcoeffs(gC10H20,gO)
       0.000201604361130571_WP , & ! Dcoeffs(gC12H25O2,gO)
       0.00020523534853619_WP , & ! Dcoeffs(gNXC12H26,gO)
       0.000194286397852819_WP , & ! Dcoeffs(gOC12H23OOH,gO)
       0.000642305387033784_WP , & ! Dcoeffs(gCH2,gO)
       0.000583610619857961_WP , & ! Dcoeffs(gHCO,gO)
       0.000642305387033784_WP , & ! Dcoeffs(gCH2D,gO)
       0.000559059055422749_WP , & ! Dcoeffs(gCH3O,gO)
       0.000506386415172215_WP , & ! Dcoeffs(gC2H3,gO)
       0.000488467158662646_WP , & ! Dcoeffs(gCH2CHO,gO)
       0.00047159101604939_WP , & ! Dcoeffs(gC2H5,gO)
       0.000371364371770769_WP , & ! Dcoeffs(gAXC3H5,gO)
       0.000339901489498369_WP , & ! Dcoeffs(gC2H3CHO,gO)
       0.000368920558473409_WP , & ! Dcoeffs(gNXC3H7,gO)
       0.000340562444284468_WP , & ! Dcoeffs(gC4H7,gO)
       0.000339207036985596_WP , & ! Dcoeffs(gPXC4H9,gO)
       0.000310382619526077_WP , & ! Dcoeffs(gPXC5H11,gO)
       0.000264851661077313_WP , & ! Dcoeffs(gPXC7H15,gO)
       0.000205287800385824_WP , & ! Dcoeffs(gPXC12H25,gO)
       0.000205287800385824_WP , & ! Dcoeffs(gS3XC12H25,gO)
       0.000205287800385824_WP , & ! Dcoeffs(gSXC12H25,gO)
       0.000201604361130571_WP , & ! Dcoeffs(gC12OOH,gO)
       0.00018687391635693_WP , & ! Dcoeffs(gO2C12H24OOH,gO)
       0.00128397259158685_WP , & ! Dcoeffs(gN2,gH2)
       0.00175129838232579_WP , & ! Dcoeffs(gO,gH2)
       0.00220012001867017_WP , & ! Dcoeffs(gH2,gH2)
       0.00372053092670794_WP , & ! Dcoeffs(gH,gH2)
       0.00174548148444458_WP , & ! Dcoeffs(gOH,gH2)
       0.00183284387910062_WP , & ! Dcoeffs(gH2O,gH2)
       0.00134242676435318_WP , & ! Dcoeffs(gH2O2,gH2)
       0.00134479062087229_WP , & ! Dcoeffs(gO2,gH2)
       0.00134357311820753_WP , & ! Dcoeffs(gHO2,gH2)
       0.00129332049502894_WP , & ! Dcoeffs(gCH2O,gH2)
       0.00121489873282622_WP , & ! Dcoeffs(gCO2,gH2)
       0.00125124813061795_WP , & ! Dcoeffs(gCH3,gH2)
       0.00127267793802287_WP , & ! Dcoeffs(gCO,gH2)
       0.00105083392223407_WP , & ! Dcoeffs(gC2H6,gH2)
       0.00126686990298269_WP , & ! Dcoeffs(gCH4,gH2)
       0.00115681237246476_WP , & ! Dcoeffs(gC2H4,gH2)
       0.0011175774455662_WP , & ! Dcoeffs(gC2H2,gH2)
       0.000869851712063682_WP , & ! Dcoeffs(gC3H6,gH2)
       0.000823914146914244_WP , & ! Dcoeffs(gC4H81,gH2)
       0.000788389618820296_WP , & ! Dcoeffs(gC5H10,gH2)
       0.000734799049707001_WP , & ! Dcoeffs(gC6H12,gH2)
       0.000692787777153306_WP , & ! Dcoeffs(gC7H14,gH2)
       0.000657977079764113_WP , & ! Dcoeffs(gC8H16,gH2)
       0.000624913953539292_WP , & ! Dcoeffs(gC9H18,gH2)
       0.000592368074973416_WP , & ! Dcoeffs(gC10H20,gH2)
       0.000530899051369924_WP , & ! Dcoeffs(gC12H25O2,gH2)
       0.000537258398625781_WP , & ! Dcoeffs(gNXC12H26,gH2)
       0.000513059144249488_WP , & ! Dcoeffs(gOC12H23OOH,gH2)
       0.0012565531454962_WP , & ! Dcoeffs(gCH2,gH2)
       0.00129473304243188_WP , & ! Dcoeffs(gHCO,gH2)
       0.0012565531454962_WP , & ! Dcoeffs(gCH2D,gH2)
       0.00125320178294477_WP , & ! Dcoeffs(gCH3O,gH2)
       0.00111607964131565_WP , & ! Dcoeffs(gC2H3,gH2)
       0.00114355723934641_WP , & ! Dcoeffs(gCH2CHO,gH2)
       0.00105197847173935_WP , & ! Dcoeffs(gC2H5,gH2)
       0.00087033962271906_WP , & ! Dcoeffs(gAXC3H5,gH2)
       0.000823924852113822_WP , & ! Dcoeffs(gC2H3CHO,gH2)
       0.000869386375913374_WP , & ! Dcoeffs(gNXC3H7,gH2)
       0.000824175536610004_WP , & ! Dcoeffs(gC4H7,gH2)
       0.000823661905417747_WP , & ! Dcoeffs(gPXC4H9,gH2)
       0.000768941661727173_WP , & ! Dcoeffs(gPXC5H11,gH2)
       0.000672987706321774_WP , & ! Dcoeffs(gPXC7H15,gH2)
       0.000537277105115026_WP , & ! Dcoeffs(gPXC12H25,gH2)
       0.000537277105115026_WP , & ! Dcoeffs(gS3XC12H25,gH2)
       0.000537277105115026_WP , & ! Dcoeffs(gSXC12H25,gH2)
       0.000530899051369924_WP , & ! Dcoeffs(gC12OOH,gH2)
       0.000494859184882681_WP , & ! Dcoeffs(gO2C12H24OOH,gH2)
       0.00237480122367031_WP , & ! Dcoeffs(gN2,gH)
       0.00335781082129878_WP , & ! Dcoeffs(gO,gH)
       0.00372053092670794_WP , & ! Dcoeffs(gH2,gH)
       0.00631276105729194_WP , & ! Dcoeffs(gH,gH)
       0.00335190850247184_WP , & ! Dcoeffs(gOH,gH)
       0.00355839723655103_WP , & ! Dcoeffs(gH2O,gH)
       0.00250972105643909_WP , & ! Dcoeffs(gH2O2,gH)
       0.00251199528478783_WP , & ! Dcoeffs(gO2,gH)
       0.00251082370261319_WP , & ! Dcoeffs(gHO2,gH)
       0.0023981922582631_WP , & ! Dcoeffs(gCH2O,gH)
       0.00224589182901713_WP , & ! Dcoeffs(gCO2,gH)
       0.00226491713687518_WP , & ! Dcoeffs(gCH3,gH)
       0.00235071262231948_WP , & ! Dcoeffs(gCO,gH)
       0.00189065102133731_WP , & ! Dcoeffs(gC2H6,gH)
       0.00230275773054468_WP , & ! Dcoeffs(gCH4,gH)
       0.00210669033262098_WP , & ! Dcoeffs(gC2H4,gH)
       0.00202194886228819_WP , & ! Dcoeffs(gC2H2,gH)
       0.00153551791112805_WP , & ! Dcoeffs(gC3H6,gH)
       0.00144991627230797_WP , & ! Dcoeffs(gC4H81,gH)
       0.00138307922069536_WP , & ! Dcoeffs(gC5H10,gH)
       0.00128044619666764_WP , & ! Dcoeffs(gC6H12,gH)
       0.00120055466250143_WP , & ! Dcoeffs(gC7H14,gH)
       0.00113474647434735_WP , & ! Dcoeffs(gC8H16,gH)
       0.00107252508098565_WP , & ! Dcoeffs(gC9H18,gH)
       0.00101159523521699_WP , & ! Dcoeffs(gC10H20,gH)
       0.000898099753942287_WP , & ! Dcoeffs(gC12H25O2,gH)
       0.000909404221351503_WP , & ! Dcoeffs(gNXC12H26,gH)
       0.000865307046269039_WP , & ! Dcoeffs(gOC12H23OOH,gH)
       0.00227002526260045_WP , & ! Dcoeffs(gCH2,gH)
       0.00239954478974831_WP , & ! Dcoeffs(gHCO,gH)
       0.00227002526260045_WP , & ! Dcoeffs(gCH2D,gH)
       0.00231413777814519_WP , & ! Dcoeffs(gCH3O,gH)
       0.00202054388179126_WP , & ! Dcoeffs(gC2H3,gH)
       0.00209462165090589_WP , & ! Dcoeffs(gCH2CHO,gH)
       0.00189171433063692_WP , & ! Dcoeffs(gC2H5,gH)
       0.00153595869200654_WP , & ! Dcoeffs(gAXC3H5,gH)
       0.00144992585804165_WP , & ! Dcoeffs(gC2H3CHO,gH)
       0.00153509763662944_WP , & ! Dcoeffs(gNXC3H7,gH)
       0.0014501503454037_WP , & ! Dcoeffs(gC4H7,gH)
       0.00144969042595602_WP , & ! Dcoeffs(gPXC4H9,gH)
       0.00134519144192546_WP , & ! Dcoeffs(gPXC5H11,gH)
       0.00116263768996465_WP , & ! Dcoeffs(gPXC7H15,gH)
       0.000909420146641455_WP , & ! Dcoeffs(gPXC12H25,gH)
       0.000909420146641455_WP , & ! Dcoeffs(gS3XC12H25,gH)
       0.000909420146641455_WP , & ! Dcoeffs(gSXC12H25,gH)
       0.000898099753942287_WP , & ! Dcoeffs(gC12OOH,gH)
       0.000831995903743664_WP , & ! Dcoeffs(gO2C12H24OOH,gH)
       0.000570515913021803_WP , & ! Dcoeffs(gN2,gOH)
       0.000867361107237895_WP , & ! Dcoeffs(gO,gOH)
       0.00174548148444458_WP , & ! Dcoeffs(gH2,gOH)
       0.00335190850247184_WP , & ! Dcoeffs(gH,gOH)
       0.000854014664603129_WP , & ! Dcoeffs(gOH,gOH)
       0.000888199336744564_WP , & ! Dcoeffs(gH2O,gOH)
       0.000580520983514641_WP , & ! Dcoeffs(gH2O2,gOH)
       0.000586584784288787_WP , & ! Dcoeffs(gO2,gOH)
       0.00058346816567506_WP , & ! Dcoeffs(gHO2,gOH)
       0.000568793219853065_WP , & ! Dcoeffs(gCH2O,gOH)
       0.000507068757755408_WP , & ! Dcoeffs(gCO2,gOH)
       0.000621606639277494_WP , & ! Dcoeffs(gCH3,gOH)
       0.000565395445027894_WP , & ! Dcoeffs(gCO,gOH)
       0.000459619671902464_WP , & ! Dcoeffs(gC2H6,gOH)
       0.000621355508422755_WP , & ! Dcoeffs(gCH4,gOH)
       0.000512532721443516_WP , & ! Dcoeffs(gC2H4,gOH)
       0.000500568764672133_WP , & ! Dcoeffs(gC2H2,gOH)
       0.00036208186123872_WP , & ! Dcoeffs(gC3H6,gOH)
       0.000331944129572735_WP , & ! Dcoeffs(gC4H81,gOH)
       0.000310968096727009_WP , & ! Dcoeffs(gC5H10,gOH)
       0.000285341835959668_WP , & ! Dcoeffs(gC6H12,gOH)
       0.00026591831706951_WP , & ! Dcoeffs(gC7H14,gOH)
       0.000250285334970907_WP , & ! Dcoeffs(gC8H16,gOH)
       0.000235969229292287_WP , & ! Dcoeffs(gC9H18,gOH)
       0.000222306365870113_WP , & ! Dcoeffs(gC10H20,gOH)
       0.000195991916812534_WP , & ! Dcoeffs(gC12H25O2,gOH)
       0.000199598416371206_WP , & ! Dcoeffs(gNXC12H26,gOH)
       0.000188849537741492_WP , & ! Dcoeffs(gOC12H23OOH,gOH)
       0.00063335188868657_WP , & ! Dcoeffs(gCH2,gOH)
       0.000572354460797411_WP , & ! Dcoeffs(gHCO,gOH)
       0.00063335188868657_WP , & ! Dcoeffs(gCH2D,gOH)
       0.000548019025603849_WP , & ! Dcoeffs(gCH3O,gOH)
       0.000496869018696843_WP , & ! Dcoeffs(gC2H3,gOH)
       0.000477798288594454_WP , & ! Dcoeffs(gCH2CHO,gOH)
       0.000462490670226356_WP , & ! Dcoeffs(gC2H5,gOH)
       0.000363358641690825_WP , & ! Dcoeffs(gAXC3H5,gOH)
       0.000331973053773158_WP , & ! Dcoeffs(gC2H3CHO,gOH)
       0.000360860616214953_WP , & ! Dcoeffs(gNXC3H7,gOH)
       0.000332649762250445_WP , & ! Dcoeffs(gC4H7,gOH)
       0.000331261980626835_WP , & ! Dcoeffs(gPXC4H9,gOH)
       0.000302780758454001_WP , & ! Dcoeffs(gPXC5H11,gOH)
       0.000258004930930048_WP , & ! Dcoeffs(gPXC7H15,gOH)
       0.000199652349138125_WP , & ! Dcoeffs(gPXC12H25,gOH)
       0.000199652349138125_WP , & ! Dcoeffs(gS3XC12H25,gOH)
       0.000199652349138125_WP , & ! Dcoeffs(gSXC12H25,gOH)
       0.000195991916812534_WP , & ! Dcoeffs(gC12OOH,gOH)
       0.000181617716402296_WP , & ! Dcoeffs(gO2C12H24OOH,gOH)
       0.000586907472353413_WP , & ! Dcoeffs(gN2,gH2O)
       0.000902476342426892_WP , & ! Dcoeffs(gO,gH2O)
       0.00183284387910062_WP , & ! Dcoeffs(gH2,gH2O)
       0.00355839723655103_WP , & ! Dcoeffs(gH,gH2O)
       0.000888199336744564_WP , & ! Dcoeffs(gOH,gH2O)
       0.000924725209969466_WP , & ! Dcoeffs(gH2O,gH2O)
       0.000597161290968115_WP , & ! Dcoeffs(gH2O2,gH2O)
       0.00060363928898236_WP , & ! Dcoeffs(gO2,gH2O)
       0.000600310107250955_WP , & ! Dcoeffs(gHO2,gH2O)
       0.000584995229107969_WP , & ! Dcoeffs(gCH2O,gH2O)
       0.000519610884706788_WP , & ! Dcoeffs(gCO2,gH2O)
       0.000641480319943879_WP , & ! Dcoeffs(gCH3,gH2O)
       0.000581518560029206_WP , & ! Dcoeffs(gCO,gH2O)
       0.000470481182328839_WP , & ! Dcoeffs(gC2H6,gH2O)
       0.000641164156176782_WP , & ! Dcoeffs(gCH4,gH2O)
       0.00052597766317215_WP , & ! Dcoeffs(gC2H4,gH2O)
       0.00051353643965648_WP , & ! Dcoeffs(gC2H2,gH2O)
       0.000368485998027465_WP , & ! Dcoeffs(gC3H6,gH2O)
       0.000336955933322258_WP , & ! Dcoeffs(gC4H81,gH2O)
       0.000315076567413716_WP , & ! Dcoeffs(gC5H10,gH2O)
       0.000288522921049236_WP , & ! Dcoeffs(gC6H12,gH2O)
       0.000268449382403498_WP , & ! Dcoeffs(gC7H14,gH2O)
       0.000252330181586298_WP , & ! Dcoeffs(gC8H16,gH2O)
       0.000237605541436434_WP , & ! Dcoeffs(gC9H18,gH2O)
       0.000223585993350154_WP , & ! Dcoeffs(gC10H20,gH2O)
       0.000196614066548498_WP , & ! Dcoeffs(gC12H25O2,gH2O)
       0.00020034171685952_WP , & ! Dcoeffs(gNXC12H26,gH2O)
       0.000189324530809433_WP , & ! Dcoeffs(gOC12H23OOH,gH2O)
       0.000653924740850306_WP , & ! Dcoeffs(gCH2,gH2O)
       0.000588793143710801_WP , & ! Dcoeffs(gHCO,gH2O)
       0.000653924740850306_WP , & ! Dcoeffs(gCH2D,gH2O)
       0.000563095822515933_WP , & ! Dcoeffs(gCH3O,gH2O)
       0.000509607383896679_WP , & ! Dcoeffs(gC2H3,gH2O)
       0.000488994214287629_WP , & ! Dcoeffs(gCH2CHO,gH2O)
       0.000473528591229947_WP , & ! Dcoeffs(gC2H5,gH2O)
       0.000369839183693048_WP , & ! Dcoeffs(gAXC3H5,gH2O)
       0.000336986611319825_WP , & ! Dcoeffs(gC2H3CHO,gH2O)
       0.000367191485877216_WP , & ! Dcoeffs(gNXC3H7,gH2O)
       0.000337704317362786_WP , & ! Dcoeffs(gC4H7,gH2O)
       0.000336232387952244_WP , & ! Dcoeffs(gPXC4H9,gH2O)
       0.000306620351098606_WP , & ! Dcoeffs(gPXC5H11,gH2O)
       0.000260320702685372_WP , & ! Dcoeffs(gPXC7H15,gH2O)
       0.000200398751464357_WP , & ! Dcoeffs(gPXC12H25,gH2O)
       0.000200398751464357_WP , & ! Dcoeffs(gS3XC12H25,gH2O)
       0.000200398751464357_WP , & ! Dcoeffs(gSXC12H25,gH2O)
       0.000196614066548498_WP , & ! Dcoeffs(gC12OOH,gH2O)
       0.000181951602962548_WP , & ! Dcoeffs(gO2C12H24OOH,gH2O)
       0.000383534746377616_WP , & ! Dcoeffs(gN2,gH2O2)
       0.00059258653884259_WP , & ! Dcoeffs(gO,gH2O2)
       0.00134242676435318_WP , & ! Dcoeffs(gH2,gH2O2)
       0.00250972105643909_WP , & ! Dcoeffs(gH,gH2O2)
       0.000580520983514641_WP , & ! Dcoeffs(gOH,gH2O2)
       0.000597161290968115_WP , & ! Dcoeffs(gH2O,gH2O2)
       0.000381914141877868_WP , & ! Dcoeffs(gH2O2,gH2O2)
       0.000387882652018554_WP , & ! Dcoeffs(gO2,gH2O2)
       0.000384818824387242_WP , & ! Dcoeffs(gHO2,gH2O2)
       0.000379762977436214_WP , & ! Dcoeffs(gCH2O,gH2O2)
       0.000329845078821777_WP , & ! Dcoeffs(gCO2,gH2O2)
       0.000442903402522169_WP , & ! Dcoeffs(gCH3,gH2O2)
       0.000380448789170606_WP , & ! Dcoeffs(gCO,gH2O2)
       0.000313155327227044_WP , & ! Dcoeffs(gC2H6,gH2O2)
       0.000439663886069346_WP , & ! Dcoeffs(gCH4,gH2O2)
       0.000348138421165653_WP , & ! Dcoeffs(gC2H4,gH2O2)
       0.000343418386068596_WP , & ! Dcoeffs(gC2H2,gH2O2)
       0.000243850404676122_WP , & ! Dcoeffs(gC3H6,gH2O2)
       0.000219609084488098_WP , & ! Dcoeffs(gC4H81,gH2O2)
       0.000203266001885732_WP , & ! Dcoeffs(gC5H10,gH2O2)
       0.000185408652242449_WP , & ! Dcoeffs(gC6H12,gH2O2)
       0.000172036906064542_WP , & ! Dcoeffs(gC7H14,gH2O2)
       0.000161407636570571_WP , & ! Dcoeffs(gC8H16,gH2O2)
       0.000151878011216899_WP , & ! Dcoeffs(gC9H18,gH2O2)
       0.000142952343913222_WP , & ! Dcoeffs(gC10H20,gH2O2)
       0.000125237271721254_WP , & ! Dcoeffs(gC12H25O2,gH2O2)
       0.00012820545955085_WP , & ! Dcoeffs(gNXC12H26,gH2O2)
       0.000120668726728134_WP , & ! Dcoeffs(gOC12H23OOH,gH2O2)
       0.000453806180071497_WP , & ! Dcoeffs(gCH2,gH2O2)
       0.000383250399475299_WP , & ! Dcoeffs(gHCO,gH2O2)
       0.000453806180071497_WP , & ! Dcoeffs(gCH2D,gH2O2)
       0.000366012903512566_WP , & ! Dcoeffs(gCH3O,gH2O2)
       0.000339773788557372_WP , & ! Dcoeffs(gC2H3,gH2O2)
       0.000313238483230046_WP , & ! Dcoeffs(gCH2CHO,gH2O2)
       0.00031602507004796_WP , & ! Dcoeffs(gC2H5,gH2O2)
       0.000245184465994154_WP , & ! Dcoeffs(gAXC3H5,gH2O2)
       0.000219640132398049_WP , & ! Dcoeffs(gC2H3CHO,gH2O2)
       0.000242571934406077_WP , & ! Dcoeffs(gNXC3H7,gH2O2)
       0.000220366048084357_WP , & ! Dcoeffs(gC4H7,gH2O2)
       0.000218876360104553_WP , & ! Dcoeffs(gPXC4H9,gH2O2)
       0.000198129908071996_WP , & ! Dcoeffs(gPXC5H11,gH2O2)
       0.000167194802475741_WP , & ! Dcoeffs(gPXC7H15,gH2O2)
       0.000128268969620989_WP , & ! Dcoeffs(gPXC12H25,gH2O2)
       0.000128268969620989_WP , & ! Dcoeffs(gS3XC12H25,gH2O2)
       0.000128268969620989_WP , & ! Dcoeffs(gSXC12H25,gH2O2)
       0.000125237271721254_WP , & ! Dcoeffs(gC12OOH,gH2O2)
       0.000116055565039968_WP , & ! Dcoeffs(gO2C12H24OOH,gH2O2)
       0.000388953289838259_WP , & ! Dcoeffs(gN2,gO2)
       0.000598528113688703_WP , & ! Dcoeffs(gO,gO2)
       0.00134479062087229_WP , & ! Dcoeffs(gH2,gO2)
       0.00251199528478783_WP , & ! Dcoeffs(gH,gO2)
       0.000586584784288787_WP , & ! Dcoeffs(gOH,gO2)
       0.00060363928898236_WP , & ! Dcoeffs(gH2O,gO2)
       0.000387882652018554_WP , & ! Dcoeffs(gH2O2,gO2)
       0.000393760703610182_WP , & ! Dcoeffs(gO2,gO2)
       0.000390742968680714_WP , & ! Dcoeffs(gHO2,gO2)
       0.000385330778800253_WP , & ! Dcoeffs(gCH2O,gO2)
       0.000335654393170014_WP , & ! Dcoeffs(gCO2,gO2)
       0.000447159127931611_WP , & ! Dcoeffs(gCH3,gO2)
       0.000385822689840133_WP , & ! Dcoeffs(gCO,gO2)
       0.000317749961201954_WP , & ! Dcoeffs(gC2H6,gO2)
       0.000444080004699237_WP , & ! Dcoeffs(gCH4,gO2)
       0.000353059945572839_WP , & ! Dcoeffs(gC2H4,gO2)
       0.000348076876556077_WP , & ! Dcoeffs(gC2H2,gO2)
       0.000248061594032902_WP , & ! Dcoeffs(gC3H6,gO2)
       0.00022387425585662_WP , & ! Dcoeffs(gC4H81,gO2)
       0.000207532800205218_WP , & ! Dcoeffs(gC5H10,gO2)
       0.000189522231103024_WP , & ! Dcoeffs(gC6H12,gO2)
       0.000176015649720003_WP , & ! Dcoeffs(gC7H14,gO2)
       0.000165263161838963_WP , & ! Dcoeffs(gC8H16,gO2)
       0.00015560101232286_WP , & ! Dcoeffs(gC9H18,gO2)
       0.000146531617168546_WP , & ! Dcoeffs(gC10H20,gO2)
       0.000128567746090024_WP , & ! Dcoeffs(gC12H25O2,gO2)
       0.000131528601098381_WP , & ! Dcoeffs(gNXC12H26,gO2)
       0.000123909755949014_WP , & ! Dcoeffs(gOC12H23OOH,gO2)
       0.000457960599613033_WP , & ! Dcoeffs(gCH2,gO2)
       0.000388768258423406_WP , & ! Dcoeffs(gHCO,gO2)
       0.000457960599613033_WP , & ! Dcoeffs(gCH2D,gO2)
       0.000371472622572972_WP , & ! Dcoeffs(gCH3O,gO2)
       0.000344481569164019_WP , & ! Dcoeffs(gC2H3,gO2)
       0.00031870232346589_WP , & ! Dcoeffs(gCH2CHO,gO2)
       0.00032057857659548_WP , & ! Dcoeffs(gC2H5,gO2)
       0.000249373127939388_WP , & ! Dcoeffs(gAXC3H5,gO2)
       0.000223904712334259_WP , & ! Dcoeffs(gC2H3CHO,gO2)
       0.000246804939040818_WP , & ! Dcoeffs(gNXC3H7,gO2)
       0.000224616846193628_WP , & ! Dcoeffs(gC4H7,gO2)
       0.000223155536472373_WP , & ! Dcoeffs(gPXC4H9,gO2)
       0.000202308031455757_WP , & ! Dcoeffs(gPXC5H11,gO2)
       0.000171071584233537_WP , & ! Dcoeffs(gPXC7H15,gO2)
       0.000131590507316762_WP , & ! Dcoeffs(gPXC12H25,gO2)
       0.000131590507316762_WP , & ! Dcoeffs(gS3XC12H25,gO2)
       0.000131590507316762_WP , & ! Dcoeffs(gSXC12H25,gO2)
       0.000128567746090024_WP , & ! Dcoeffs(gC12OOH,gO2)
       0.000119203463484425_WP , & ! Dcoeffs(gO2C12H24OOH,gO2)
       0.000386170777248283_WP , & ! Dcoeffs(gN2,gHO2)
       0.00059547400789023_WP , & ! Dcoeffs(gO,gHO2)
       0.00134357311820753_WP , & ! Dcoeffs(gH2,gHO2)
       0.00251082370261319_WP , & ! Dcoeffs(gH,gHO2)
       0.00058346816567506_WP , & ! Dcoeffs(gOH,gHO2)
       0.000600310107250955_WP , & ! Dcoeffs(gH2O,gHO2)
       0.000384818824387242_WP , & ! Dcoeffs(gH2O2,gHO2)
       0.000390742968680714_WP , & ! Dcoeffs(gO2,gHO2)
       0.000387701745468407_WP , & ! Dcoeffs(gHO2,gHO2)
       0.00038247198549731_WP , & ! Dcoeffs(gCH2O,gHO2)
       0.000332673702485061_WP , & ! Dcoeffs(gCO2,gHO2)
       0.000444971367473365_WP , & ! Dcoeffs(gCH3,gHO2)
       0.00038306310025145_WP , & ! Dcoeffs(gCO,gHO2)
       0.000315390847803442_WP , & ! Dcoeffs(gC2H6,gHO2)
       0.000441810028116574_WP , & ! Dcoeffs(gCH4,gHO2)
       0.000350532665893569_WP , & ! Dcoeffs(gC2H4,gHO2)
       0.000345684340780731_WP , & ! Dcoeffs(gC2H2,gHO2)
       0.000245900705230394_WP , & ! Dcoeffs(gC3H6,gHO2)
       0.000221686793370783_WP , & ! Dcoeffs(gC4H81,gHO2)
       0.00020534532344366_WP , & ! Dcoeffs(gC5H10,gHO2)
       0.000187413907348326_WP , & ! Dcoeffs(gC6H12,gHO2)
       0.000173976890042496_WP , & ! Dcoeffs(gC7H14,gHO2)
       0.000163287898372443_WP , & ! Dcoeffs(gC8H16,gHO2)
       0.000153693928073407_WP , & ! Dcoeffs(gC9H18,gHO2)
       0.000144698385783289_WP , & ! Dcoeffs(gC10H20,gHO2)
       0.000126862575292341_WP , & ! Dcoeffs(gC12H25O2,gHO2)
       0.000129826912395245_WP , & ! Dcoeffs(gNXC12H26,gHO2)
       0.000122250484954982_WP , & ! Dcoeffs(gOC12H23OOH,gHO2)
       0.000455824684474729_WP , & ! Dcoeffs(gCH2,gHO2)
       0.000385934928915501_WP , & ! Dcoeffs(gHCO,gHO2)
       0.000455824684474729_WP , & ! Dcoeffs(gCH2D,gHO2)
       0.000368669495958801_WP , & ! Dcoeffs(gCH3O,gHO2)
       0.000342063887251606_WP , & ! Dcoeffs(gC2H3,gHO2)
       0.000315898777976519_WP , & ! Dcoeffs(gCH2CHO,gHO2)
       0.000318240432387707_WP , & ! Dcoeffs(gC2H5,gHO2)
       0.000247223703024073_WP , & ! Dcoeffs(gAXC3H5,gHO2)
       0.000221717550331841_WP , & ! Dcoeffs(gC2H3CHO,gHO2)
       0.000244632950220536_WP , & ! Dcoeffs(gNXC3H7,gHO2)
       0.000222436686528183_WP , & ! Dcoeffs(gC4H7,gHO2)
       0.000220960958945099_WP , & ! Dcoeffs(gPXC4H9,gHO2)
       0.000200166065415723_WP , & ! Dcoeffs(gPXC5H11,gHO2)
       0.000169085099529598_WP , & ! Dcoeffs(gPXC7H15,gHO2)
       0.000129889629651748_WP , & ! Dcoeffs(gPXC12H25,gHO2)
       0.000129889629651748_WP , & ! Dcoeffs(gS3XC12H25,gHO2)
       0.000129889629651748_WP , & ! Dcoeffs(gSXC12H25,gHO2)
       0.000126862575292341_WP , & ! Dcoeffs(gC12OOH,gHO2)
       0.000117591973093396_WP , & ! Dcoeffs(gO2C12H24OOH,gHO2)
       0.000380552592543488_WP , & ! Dcoeffs(gN2,gCH2O)
       0.000580118482525006_WP , & ! Dcoeffs(gO,gCH2O)
       0.00129332049502894_WP , & ! Dcoeffs(gH2,gCH2O)
       0.0023981922582631_WP , & ! Dcoeffs(gH,gCH2O)
       0.000568793219853065_WP , & ! Dcoeffs(gOH,gCH2O)
       0.000584995229107969_WP , & ! Dcoeffs(gH2O,gCH2O)
       0.000379762977436214_WP , & ! Dcoeffs(gH2O2,gCH2O)
       0.000385330778800253_WP , & ! Dcoeffs(gO2,gCH2O)
       0.00038247198549731_WP , & ! Dcoeffs(gHO2,gCH2O)
       0.000377154854581301_WP , & ! Dcoeffs(gCH2O,gCH2O)
       0.000329814883398927_WP , & ! Dcoeffs(gCO2,gCH2O)
       0.000435835936461339_WP , & ! Dcoeffs(gCH3,gCH2O)
       0.000377544929520638_WP , & ! Dcoeffs(gCO,gCH2O)
       0.000312063320426938_WP , & ! Dcoeffs(gC2H6,gCH2O)
       0.00043291772800062_WP , & ! Dcoeffs(gCH4,gCH2O)
       0.000346034202957529_WP , & ! Dcoeffs(gC2H4,gCH2O)
       0.000341152120676318_WP , & ! Dcoeffs(gC2H2,gCH2O)
       0.000244929672868588_WP , & ! Dcoeffs(gC3H6,gCH2O)
       0.000221682275566736_WP , & ! Dcoeffs(gC4H81,gCH2O)
       0.000205940958771633_WP , & ! Dcoeffs(gC5H10,gCH2O)
       0.000188467498229115_WP , & ! Dcoeffs(gC6H12,gCH2O)
       0.000175332416290554_WP , & ! Dcoeffs(gC7H14,gCH2O)
       0.000164852614692269_WP , & ! Dcoeffs(gC8H16,gCH2O)
       0.000155409810727048_WP , & ! Dcoeffs(gC9H18,gCH2O)
       0.000146522753837095_WP , & ! Dcoeffs(gC10H20,gCH2O)
       0.000128920870476707_WP , & ! Dcoeffs(gC12H25O2,gCH2O)
       0.000131786601791341_WP , & ! Dcoeffs(gNXC12H26,gCH2O)
       0.000124331001766918_WP , & ! Dcoeffs(gOC12H23OOH,gCH2O)
       0.00044614970894825_WP , & ! Dcoeffs(gCH2,gCH2O)
       0.000380416067601609_WP , & ! Dcoeffs(gHCO,gCH2O)
       0.00044614970894825_WP , & ! Dcoeffs(gCH2D,gCH2O)
       0.000363873439761536_WP , & ! Dcoeffs(gCH3O,gCH2O)
       0.000337729802341268_WP , & ! Dcoeffs(gC2H3,gCH2O)
       0.000313417501445691_WP , & ! Dcoeffs(gCH2CHO,gCH2O)
       0.000314755936351615_WP , & ! Dcoeffs(gC2H5,gCH2O)
       0.000246178148028855_WP , & ! Dcoeffs(gAXC3H5,gCH2O)
       0.000221711222086638_WP , & ! Dcoeffs(gC2H3CHO,gCH2O)
       0.000243733660892026_WP , & ! Dcoeffs(gNXC3H7,gCH2O)
       0.000222388095045878_WP , & ! Dcoeffs(gC4H7,gCH2O)
       0.000220999234595445_WP , & ! Dcoeffs(gPXC4H9,gCH2O)
       0.000200844271802073_WP , & ! Dcoeffs(gPXC5H11,gCH2O)
       0.0001704819349292_WP , & ! Dcoeffs(gPXC7H15,gCH2O)
       0.000131845377268614_WP , & ! Dcoeffs(gPXC12H25,gCH2O)
       0.000131845377268614_WP , & ! Dcoeffs(gS3XC12H25,gCH2O)
       0.000131845377268614_WP , & ! Dcoeffs(gSXC12H25,gCH2O)
       0.000128920870476707_WP , & ! Dcoeffs(gC12OOH,gCH2O)
       0.000119688455847605_WP , & ! Dcoeffs(gO2C12H24OOH,gCH2O)
       0.000333938055929546_WP , & ! Dcoeffs(gN2,gCO2)
       0.000518461264040031_WP , & ! Dcoeffs(gO,gCO2)
       0.00121489873282622_WP , & ! Dcoeffs(gH2,gCO2)
       0.00224589182901713_WP , & ! Dcoeffs(gH,gCO2)
       0.000507068757755408_WP , & ! Dcoeffs(gOH,gCO2)
       0.000519610884706788_WP , & ! Dcoeffs(gH2O,gCO2)
       0.000329845078821777_WP , & ! Dcoeffs(gH2O2,gCO2)
       0.000335654393170014_WP , & ! Dcoeffs(gO2,gCO2)
       0.000332673702485061_WP , & ! Dcoeffs(gHO2,gCO2)
       0.000329814883398927_WP , & ! Dcoeffs(gCH2O,gCO2)
       0.000283539271059502_WP , & ! Dcoeffs(gCO2,gCO2)
       0.000393449557024219_WP , & ! Dcoeffs(gCH3,gCO2)
       0.000331366539576951_WP , & ! Dcoeffs(gCO,gCO2)
       0.000274037665192637_WP , & ! Dcoeffs(gC2H6,gCO2)
       0.000389669955527982_WP , & ! Dcoeffs(gCH4,gCO2)
       0.000304291312685862_WP , & ! Dcoeffs(gC2H4,gCO2)
       0.000301269131716291_WP , & ! Dcoeffs(gC2H2,gCO2)
       0.000212398177327266_WP , & ! Dcoeffs(gC3H6,gCO2)
       0.000189845045687207_WP , & ! Dcoeffs(gC4H81,gCO2)
       0.000174756670981324_WP , & ! Dcoeffs(gC5H10,gCO2)
       0.000158908394652196_WP , & ! Dcoeffs(gC6H12,gCO2)
       0.000147090401095983_WP , & ! Dcoeffs(gC7H14,gCO2)
       0.000137740675445044_WP , & ! Dcoeffs(gC8H16,gCO2)
       0.000129433824316387_WP , & ! Dcoeffs(gC9H18,gCO2)
       0.000121720008225509_WP , & ! Dcoeffs(gC10H20,gCO2)
       0.000106193774124665_WP , & ! Dcoeffs(gC12H25O2,gCO2)
       0.00010901388678168_WP , & ! Dcoeffs(gNXC12H26,gCO2)
       0.000102286517995063_WP , & ! Dcoeffs(gOC12H23OOH,gCO2)
       0.000403850166800589_WP , & ! Dcoeffs(gCH2,gCO2)
       0.00033320267702311_WP , & ! Dcoeffs(gHCO,gCO2)
       0.000403850166800589_WP , & ! Dcoeffs(gCH2D,gCO2)
       0.000317909507080548_WP , & ! Dcoeffs(gCH3O,gCO2)
       0.000297720609563858_WP , & ! Dcoeffs(gC2H3,gCO2)
       0.000270065235384694_WP , & ! Dcoeffs(gCH2CHO,gCO2)
       0.000276846893012916_WP , & ! Dcoeffs(gC2H5,gCO2)
       0.000213726517927594_WP , & ! Dcoeffs(gAXC3H5,gCO2)
       0.000189876304377523_WP , & ! Dcoeffs(gC2H3CHO,gCO2)
       0.000211124209957998_WP , & ! Dcoeffs(gNXC3H7,gCO2)
       0.000190606941691428_WP , & ! Dcoeffs(gC4H7,gCO2)
       0.00018910713492654_WP , & ! Dcoeffs(gPXC4H9,gCO2)
       0.000170401495286731_WP , & ! Dcoeffs(gPXC5H11,gCO2)
       0.000143034840843528_WP , & ! Dcoeffs(gPXC7H15,gCO2)
       0.000109080494359155_WP , & ! Dcoeffs(gPXC12H25,gCO2)
       0.000109080494359155_WP , & ! Dcoeffs(gS3XC12H25,gCO2)
       0.000109080494359155_WP , & ! Dcoeffs(gSXC12H25,gCO2)
       0.000106193774124665_WP , & ! Dcoeffs(gC12OOH,gCO2)
       9.83475118109865e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gCO2)
       0.000437333639705725_WP , & ! Dcoeffs(gN2,gCH3)
       0.000630726889631031_WP , & ! Dcoeffs(gO,gCH3)
       0.00125124813061795_WP , & ! Dcoeffs(gH2,gCH3)
       0.00226491713687518_WP , & ! Dcoeffs(gH,gCH3)
       0.000621606639277494_WP , & ! Dcoeffs(gOH,gCH3)
       0.000641480319943879_WP , & ! Dcoeffs(gH2O,gCH3)
       0.000442903402522169_WP , & ! Dcoeffs(gH2O2,gCH3)
       0.000447159127931611_WP , & ! Dcoeffs(gO2,gCH3)
       0.000444971367473365_WP , & ! Dcoeffs(gHO2,gCH3)
       0.000435835936461339_WP , & ! Dcoeffs(gCH2O,gCH3)
       0.000393449557024219_WP , & ! Dcoeffs(gCO2,gCH3)
       0.000475721602590614_WP , & ! Dcoeffs(gCH3,gCH3)
       0.000433962568930371_WP , & ! Dcoeffs(gCO,gCH3)
       0.000362515192376144_WP , & ! Dcoeffs(gC2H6,gCH3)
       0.000474913766296317_WP , & ! Dcoeffs(gCH4,gCH3)
       0.000398746987939258_WP , & ! Dcoeffs(gC2H4,gCH3)
       0.000391008408985081_WP , & ! Dcoeffs(gC2H2,gCH3)
       0.000293504209433033_WP , & ! Dcoeffs(gC3H6,gCH3)
       0.000271552174456435_WP , & ! Dcoeffs(gC4H81,gCH3)
       0.000256187969908242_WP , & ! Dcoeffs(gC5H10,gCH3)
       0.000237310697023796_WP , & ! Dcoeffs(gC6H12,gCH3)
       0.000222849316681022_WP , & ! Dcoeffs(gC7H14,gCH3)
       0.000211104749051153_WP , & ! Dcoeffs(gC8H16,gCH3)
       0.000200259974075612_WP , & ! Dcoeffs(gC9H18,gCH3)
       0.000189822700784405_WP , & ! Dcoeffs(gC10H20,gCH3)
       0.000169469562465148_WP , & ! Dcoeffs(gC12H25O2,gCH3)
       0.00017227137081761_WP , & ! Dcoeffs(gNXC12H26,gCH3)
       0.000163879696584055_WP , & ! Dcoeffs(gOC12H23OOH,gCH3)
       0.000484193286057581_WP , & ! Dcoeffs(gCH2,gCH3)
       0.000438354289090043_WP , & ! Dcoeffs(gHCO,gCH3)
       0.000484193286057581_WP , & ! Dcoeffs(gCH2D,gCH3)
       0.000421970631469897_WP , & ! Dcoeffs(gCH3O,gCH3)
       0.000388331802303509_WP , & ! Dcoeffs(gC2H3,gCH3)
       0.000373828611114409_WP , & ! Dcoeffs(gCH2CHO,gCH3)
       0.000364604921198876_WP , & ! Dcoeffs(gC2H5,gCH3)
       0.000294450811767392_WP , & ! Dcoeffs(gAXC3H5,gCH3)
       0.000271573670557643_WP , & ! Dcoeffs(gC2H3CHO,gCH3)
       0.000292599047949749_WP , & ! Dcoeffs(gNXC3H7,gCH3)
       0.000272076640600057_WP , & ! Dcoeffs(gC4H7,gCH3)
       0.000271045259749179_WP , & ! Dcoeffs(gPXC4H9,gCH3)
       0.00025019780185268_WP , & ! Dcoeffs(gPXC5H11,gCH3)
       0.000216921796240202_WP , & ! Dcoeffs(gPXC7H15,gCH3)
       0.000172312955827666_WP , & ! Dcoeffs(gPXC12H25,gCH3)
       0.000172312955827666_WP , & ! Dcoeffs(gS3XC12H25,gCH3)
       0.000172312955827666_WP , & ! Dcoeffs(gSXC12H25,gCH3)
       0.000169469562465148_WP , & ! Dcoeffs(gC12OOH,gCH3)
       0.000158190108342894_WP , & ! Dcoeffs(gO2C12H24OOH,gCH3)
       0.00038074458458987_WP , & ! Dcoeffs(gN2,gCO)
       0.00057637020671351_WP , & ! Dcoeffs(gO,gCO)
       0.00127267793802287_WP , & ! Dcoeffs(gH2,gCO)
       0.00235071262231948_WP , & ! Dcoeffs(gH,gCO)
       0.000565395445027894_WP , & ! Dcoeffs(gOH,gCO)
       0.000581518560029206_WP , & ! Dcoeffs(gH2O,gCO)
       0.000380448789170606_WP , & ! Dcoeffs(gH2O2,gCO)
       0.000385822689840133_WP , & ! Dcoeffs(gO2,gCO)
       0.00038306310025145_WP , & ! Dcoeffs(gHO2,gCO)
       0.000377544929520638_WP , & ! Dcoeffs(gCH2O,gCO)
       0.000331366539576951_WP , & ! Dcoeffs(gCO2,gCO)
       0.000433962568930371_WP , & ! Dcoeffs(gCH3,gCO)
       0.000377759205307997_WP , & ! Dcoeffs(gCO,gCO)
       0.000312857514806277_WP , & ! Dcoeffs(gC2H6,gCO)
       0.000431226590639635_WP , & ! Dcoeffs(gCH4,gCO)
       0.000346476859047397_WP , & ! Dcoeffs(gC2H4,gCO)
       0.000341457786369011_WP , & ! Dcoeffs(gC2H2,gCO)
       0.00024655692454695_WP , & ! Dcoeffs(gC3H6,gCO)
       0.000223745939423743_WP , & ! Dcoeffs(gC4H81,gCO)
       0.000208259244917311_WP , & ! Dcoeffs(gC5H10,gCO)
       0.000190908434346346_WP , & ! Dcoeffs(gC6H12,gCO)
       0.000177838013537895_WP , & ! Dcoeffs(gC7H14,gCO)
       0.000167388847996724_WP , & ! Dcoeffs(gC8H16,gCO)
       0.000157947819571172_WP , & ! Dcoeffs(gC9H18,gCO)
       0.000149039288940324_WP , & ! Dcoeffs(gC10H20,gCO)
       0.000131417814874274_WP , & ! Dcoeffs(gC12H25O2,gCO)
       0.000134239933465597_WP , & ! Dcoeffs(gNXC12H26,gCO)
       0.000126795223114576_WP , & ! Dcoeffs(gOC12H23OOH,gCO)
       0.000443993904098287_WP , & ! Dcoeffs(gCH2,gCO)
       0.00038069658269363_WP , & ! Dcoeffs(gHCO,gCO)
       0.000443993904098287_WP , & ! Dcoeffs(gCH2D,gCO)
       0.000364437170891124_WP , & ! Dcoeffs(gCH3O,gCO)
       0.000338143737917509_WP , & ! Dcoeffs(gC2H3,gCO)
       0.000314973336046677_WP , & ! Dcoeffs(gCH2CHO,gCO)
       0.000315463536043639_WP , & ! Dcoeffs(gC2H5,gCO)
       0.000247763158154071_WP , & ! Dcoeffs(gAXC3H5,gCO)
       0.000223773847095675_WP , & ! Dcoeffs(gC2H3CHO,gCO)
       0.000245401611429231_WP , & ! Dcoeffs(gNXC3H7,gCO)
       0.000224426474459157_WP , & ! Dcoeffs(gC4H7,gCO)
       0.0002230874592799_WP , & ! Dcoeffs(gPXC4H9,gCO)
       0.00020315687566181_WP , & ! Dcoeffs(gPXC5H11,gCO)
       0.000172958057907887_WP , & ! Dcoeffs(gPXC7H15,gCO)
       0.000134296351757198_WP , & ! Dcoeffs(gPXC12H25,gCO)
       0.000134296351757198_WP , & ! Dcoeffs(gS3XC12H25,gCO)
       0.000134296351757198_WP , & ! Dcoeffs(gSXC12H25,gCO)
       0.000131417814874274_WP , & ! Dcoeffs(gC12OOH,gCO)
       0.000122115526183795_WP , & ! Dcoeffs(gO2C12H24OOH,gCO)
       0.000315122850778215_WP , & ! Dcoeffs(gN2,gC2H6)
       0.000468775755742338_WP , & ! Dcoeffs(gO,gC2H6)
       0.00105083392223407_WP , & ! Dcoeffs(gH2,gC2H6)
       0.00189065102133731_WP , & ! Dcoeffs(gH,gC2H6)
       0.000459619671902464_WP , & ! Dcoeffs(gOH,gC2H6)
       0.000470481182328839_WP , & ! Dcoeffs(gH2O,gC2H6)
       0.000313155327227044_WP , & ! Dcoeffs(gH2O2,gC2H6)
       0.000317749961201954_WP , & ! Dcoeffs(gO2,gC2H6)
       0.000315390847803442_WP , & ! Dcoeffs(gHO2,gC2H6)
       0.000312063320426938_WP , & ! Dcoeffs(gCH2O,gC2H6)
       0.000274037665192637_WP , & ! Dcoeffs(gCO2,gC2H6)
       0.000362515192376144_WP , & ! Dcoeffs(gCH3,gC2H6)
       0.000312857514806277_WP , & ! Dcoeffs(gCO,gC2H6)
       0.000262460684141593_WP , & ! Dcoeffs(gC2H6,gC2H6)
       0.000359618836690631_WP , & ! Dcoeffs(gCH4,gC2H6)
       0.000288938158921507_WP , & ! Dcoeffs(gC2H4,gC2H6)
       0.000285689638328236_WP , & ! Dcoeffs(gC2H2,gC2H6)
       0.000208717268586092_WP , & ! Dcoeffs(gC3H6,gC2H6)
       0.000189540851293258_WP , & ! Dcoeffs(gC4H81,gC2H6)
       0.000176568515512598_WP , & ! Dcoeffs(gC5H10,gC2H6)
       0.000162325857215685_WP , & ! Dcoeffs(gC6H12,gC2H6)
       0.000151578609467526_WP , & ! Dcoeffs(gC7H14,gC2H6)
       0.000142978348342039_WP , & ! Dcoeffs(gC8H16,gC2H6)
       0.000135217384133035_WP , & ! Dcoeffs(gC9H18,gC2H6)
       0.000127899361856981_WP , & ! Dcoeffs(gC10H20,gC2H6)
       0.000113256360163701_WP , & ! Dcoeffs(gC12H25O2,gC2H6)
       0.000115700802420398_WP , & ! Dcoeffs(gNXC12H26,gC2H6)
       0.000109438588193007_WP , & ! Dcoeffs(gOC12H23OOH,gC2H6)
       0.000371097831056395_WP , & ! Dcoeffs(gCH2,gC2H6)
       0.000314763572537127_WP , & ! Dcoeffs(gHCO,gC2H6)
       0.000371097831056395_WP , & ! Dcoeffs(gCH2D,gC2H6)
       0.00030181995272476_WP , & ! Dcoeffs(gCH3O,gC2H6)
       0.000282821830348657_WP , & ! Dcoeffs(gC2H3,gC2H6)
       0.000261677891312613_WP , & ! Dcoeffs(gCH2CHO,gC2H6)
       0.000264726884369387_WP , & ! Dcoeffs(gC2H5,gC2H6)
       0.000209782024717738_WP , & ! Dcoeffs(gAXC3H5,gC2H6)
       0.000189565623433162_WP , & ! Dcoeffs(gC2H3CHO,gC2H6)
       0.00020769725128058_WP , & ! Dcoeffs(gNXC3H7,gC2H6)
       0.000190144883650643_WP , & ! Dcoeffs(gC4H7,gC2H6)
       0.000188956310858792_WP , & ! Dcoeffs(gPXC4H9,gC2H6)
       0.000172488614859723_WP , & ! Dcoeffs(gPXC5H11,gC2H6)
       0.000147660575481868_WP , & ! Dcoeffs(gPXC7H15,gC2H6)
       0.000115752465131257_WP , & ! Dcoeffs(gPXC12H25,gC2H6)
       0.000115752465131257_WP , & ! Dcoeffs(gS3XC12H25,gC2H6)
       0.000115752465131257_WP , & ! Dcoeffs(gSXC12H25,gC2H6)
       0.000113256360163701_WP , & ! Dcoeffs(gC12OOH,gC2H6)
       0.000105567755934202_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H6)
       0.000434600052868636_WP , & ! Dcoeffs(gN2,gCH4)
       0.000630784279408771_WP , & ! Dcoeffs(gO,gCH4)
       0.00126686990298269_WP , & ! Dcoeffs(gH2,gCH4)
       0.00230275773054468_WP , & ! Dcoeffs(gH,gCH4)
       0.000621355508422755_WP , & ! Dcoeffs(gOH,gCH4)
       0.000641164156176782_WP , & ! Dcoeffs(gH2O,gCH4)
       0.000439663886069346_WP , & ! Dcoeffs(gH2O2,gCH4)
       0.000444080004699237_WP , & ! Dcoeffs(gO2,gCH4)
       0.000441810028116574_WP , & ! Dcoeffs(gHO2,gCH4)
       0.00043291772800062_WP , & ! Dcoeffs(gCH2O,gCH4)
       0.000389669955527982_WP , & ! Dcoeffs(gCO2,gCH4)
       0.000474913766296317_WP , & ! Dcoeffs(gCH3,gCH4)
       0.000431226590639635_WP , & ! Dcoeffs(gCO,gCH4)
       0.000359618836690631_WP , & ! Dcoeffs(gC2H6,gCH4)
       0.000473906360242553_WP , & ! Dcoeffs(gCH4,gCH4)
       0.000395989718657373_WP , & ! Dcoeffs(gC2H4,gCH4)
       0.000388433682877806_WP , & ! Dcoeffs(gC2H2,gCH4)
       0.000290187606194083_WP , & ! Dcoeffs(gC3H6,gCH4)
       0.000267952525621614_WP , & ! Dcoeffs(gC4H81,gCH4)
       0.000252445484657336_WP , & ! Dcoeffs(gC5H10,gCH4)
       0.000233566290072705_WP , & ! Dcoeffs(gC6H12,gCH4)
       0.000219131654013742_WP , & ! Dcoeffs(gC7H14,gCH4)
       0.000207429342225985_WP , & ! Dcoeffs(gC8H16,gCH4)
       0.000196647401127184_WP , & ! Dcoeffs(gC9H18,gCH4)
       0.000186291240861066_WP , & ! Dcoeffs(gC10H20,gCH4)
       0.000166081811727536_WP , & ! Dcoeffs(gC12H25O2,gCH4)
       0.000168901576001868_WP , & ! Dcoeffs(gNXC12H26,gCH4)
       0.000160554869189162_WP , & ! Dcoeffs(gOC12H23OOH,gCH4)
       0.000483642913476372_WP , & ! Dcoeffs(gCH2,gCH4)
       0.000435528206761748_WP , & ! Dcoeffs(gHCO,gCH4)
       0.000483642913476372_WP , & ! Dcoeffs(gCH2D,gCH4)
       0.000418962559358755_WP , & ! Dcoeffs(gCH3O,gCH4)
       0.000385663993734222_WP , & ! Dcoeffs(gC2H3,gCH4)
       0.000370147331429965_WP , & ! Dcoeffs(gCH2CHO,gCH4)
       0.00036178223453537_WP , & ! Dcoeffs(gC2H5,gCH4)
       0.000291168865851034_WP , & ! Dcoeffs(gAXC3H5,gCH4)
       0.000267974842668811_WP , & ! Dcoeffs(gC2H3CHO,gCH4)
       0.000289249160962092_WP , & ! Dcoeffs(gNXC3H7,gCH4)
       0.00026849699513202_WP , & ! Dcoeffs(gC4H7,gCH4)
       0.000267426224860914_WP , & ! Dcoeffs(gPXC4H9,gCH4)
       0.0002464933183948_WP , & ! Dcoeffs(gPXC5H11,gCH4)
       0.00021326345965475_WP , & ! Dcoeffs(gPXC7H15,gCH4)
       0.000168944845599797_WP , & ! Dcoeffs(gPXC12H25,gCH4)
       0.000168944845599797_WP , & ! Dcoeffs(gS3XC12H25,gCH4)
       0.000168944845599797_WP , & ! Dcoeffs(gSXC12H25,gCH4)
       0.000166081811727536_WP , & ! Dcoeffs(gC12OOH,gCH4)
       0.000154932866960863_WP , & ! Dcoeffs(gO2C12H24OOH,gCH4)
       0.000349097691543822_WP , & ! Dcoeffs(gN2,gC2H4)
       0.000522486953956379_WP , & ! Dcoeffs(gO,gC2H4)
       0.00115681237246476_WP , & ! Dcoeffs(gH2,gC2H4)
       0.00210669033262098_WP , & ! Dcoeffs(gH,gC2H4)
       0.000512532721443516_WP , & ! Dcoeffs(gOH,gC2H4)
       0.00052597766317215_WP , & ! Dcoeffs(gH2O,gC2H4)
       0.000348138421165653_WP , & ! Dcoeffs(gH2O2,gC2H4)
       0.000353059945572839_WP , & ! Dcoeffs(gO2,gC2H4)
       0.000350532665893569_WP , & ! Dcoeffs(gHO2,gC2H4)
       0.000346034202957529_WP , & ! Dcoeffs(gCH2O,gC2H4)
       0.000304291312685862_WP , & ! Dcoeffs(gCO2,gC2H4)
       0.000398746987939258_WP , & ! Dcoeffs(gCH3,gC2H4)
       0.000346476859047397_WP , & ! Dcoeffs(gCO,gC2H4)
       0.000288938158921507_WP , & ! Dcoeffs(gC2H6,gC2H4)
       0.000395989718657373_WP , & ! Dcoeffs(gCH4,gC2H4)
       0.000318915516716172_WP , & ! Dcoeffs(gC2H4,gC2H4)
       0.000314723413685897_WP , & ! Dcoeffs(gC2H2,gC2H4)
       0.000229090786719459_WP , & ! Dcoeffs(gC3H6,gC2H4)
       0.000208213406323774_WP , & ! Dcoeffs(gC4H81,gC2H4)
       0.000194046239033776_WP , & ! Dcoeffs(gC5H10,gC2H4)
       0.000178253391975693_WP , & ! Dcoeffs(gC6H12,gC2H4)
       0.000166335434384876_WP , & ! Dcoeffs(gC7H14,gC2H4)
       0.000156794286214216_WP , & ! Dcoeffs(gC8H16,gC2H4)
       0.00014816731624933_WP , & ! Dcoeffs(gC9H18,gC2H4)
       0.000140019478624668_WP , & ! Dcoeffs(gC10H20,gC2H4)
       0.000123827344024322_WP , & ! Dcoeffs(gC12H25O2,gC2H4)
       0.000126450572878396_WP , & ! Dcoeffs(gNXC12H26,gC2H4)
       0.000119579587322224_WP , & ! Dcoeffs(gOC12H23OOH,gC2H4)
       0.00040796905944982_WP , & ! Dcoeffs(gCH2,gC2H4)
       0.000348925042271393_WP , & ! Dcoeffs(gHCO,gC2H4)
       0.00040796905944982_WP , & ! Dcoeffs(gCH2D,gC2H4)
       0.000334405113922974_WP , & ! Dcoeffs(gCH3O,gC2H4)
       0.000311666622462626_WP , & ! Dcoeffs(gC2H3,gC2H4)
       0.000289892045039247_WP , & ! Dcoeffs(gCH2CHO,gC2H4)
       0.000291346797495705_WP , & ! Dcoeffs(gC2H5,gC2H4)
       0.000230212576404801_WP , & ! Dcoeffs(gAXC3H5,gC2H4)
       0.000208239402593315_WP , & ! Dcoeffs(gC2H3CHO,gC2H4)
       0.000228016348141257_WP , & ! Dcoeffs(gNXC3H7,gC2H4)
       0.000208847330448471_WP , & ! Dcoeffs(gC4H7,gC2H4)
       0.000207600024681994_WP , & ! Dcoeffs(gPXC4H9,gC2H4)
       0.000189439543158629_WP , & ! Dcoeffs(gPXC5H11,gC2H4)
       0.000161910833074417_WP , & ! Dcoeffs(gPXC7H15,gC2H4)
       0.000126503785867108_WP , & ! Dcoeffs(gPXC12H25,gC2H4)
       0.000126503785867108_WP , & ! Dcoeffs(gS3XC12H25,gC2H4)
       0.000126503785867108_WP , & ! Dcoeffs(gSXC12H25,gC2H4)
       0.000123827344024322_WP , & ! Dcoeffs(gC12OOH,gC2H4)
       0.000115274567391594_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H4)
       0.000343998052653477_WP , & ! Dcoeffs(gN2,gC2H2)
       0.000510017125100313_WP , & ! Dcoeffs(gO,gC2H2)
       0.0011175774455662_WP , & ! Dcoeffs(gH2,gC2H2)
       0.00202194886228819_WP , & ! Dcoeffs(gH,gC2H2)
       0.000500568764672133_WP , & ! Dcoeffs(gOH,gC2H2)
       0.00051353643965648_WP , & ! Dcoeffs(gH2O,gC2H2)
       0.000343418386068596_WP , & ! Dcoeffs(gH2O2,gC2H2)
       0.000348076876556077_WP , & ! Dcoeffs(gO2,gC2H2)
       0.000345684340780731_WP , & ! Dcoeffs(gHO2,gC2H2)
       0.000341152120676318_WP , & ! Dcoeffs(gCH2O,gC2H2)
       0.000301269131716291_WP , & ! Dcoeffs(gCO2,gC2H2)
       0.000391008408985081_WP , & ! Dcoeffs(gCH3,gC2H2)
       0.000341457786369011_WP , & ! Dcoeffs(gCO,gC2H2)
       0.000285689638328236_WP , & ! Dcoeffs(gC2H6,gC2H2)
       0.000388433682877806_WP , & ! Dcoeffs(gCH4,gC2H2)
       0.000314723413685897_WP , & ! Dcoeffs(gC2H4,gC2H2)
       0.000310529235825414_WP , & ! Dcoeffs(gC2H2,gC2H2)
       0.000227741855257729_WP , & ! Dcoeffs(gC3H6,gC2H2)
       0.00020762237654515_WP , & ! Dcoeffs(gC4H81,gC2H2)
       0.00019393047709591_WP , & ! Dcoeffs(gC5H10,gC2H2)
       0.000178525627390824_WP , & ! Dcoeffs(gC6H12,gC2H2)
       0.000166870147382517_WP , & ! Dcoeffs(gC7H14,gC2H2)
       0.000157516514365395_WP , & ! Dcoeffs(gC8H16,gC2H2)
       0.000149032954966575_WP , & ! Dcoeffs(gC9H18,gC2H2)
       0.000140996831322474_WP , & ! Dcoeffs(gC10H20,gC2H2)
       0.000125035637840811_WP , & ! Dcoeffs(gC12H25O2,gC2H2)
       0.000127581325785075_WP , & ! Dcoeffs(gNXC12H26,gC2H2)
       0.000120821587251503_WP , & ! Dcoeffs(gOC12H23OOH,gC2H2)
       0.000399816225585138_WP , & ! Dcoeffs(gCH2,gC2H2)
       0.000343892910678475_WP , & ! Dcoeffs(gHCO,gC2H2)
       0.000399816225585138_WP , & ! Dcoeffs(gCH2D,gC2H2)
       0.000329932687416168_WP , & ! Dcoeffs(gCH3O,gC2H2)
       0.00030762206998283_WP , & ! Dcoeffs(gC2H3,gC2H2)
       0.000287202347347334_WP , & ! Dcoeffs(gCH2CHO,gC2H2)
       0.000287979834044314_WP , & ! Dcoeffs(gC2H5,gC2H2)
       0.00022880764501909_WP , & ! Dcoeffs(gAXC3H5,gC2H2)
       0.000207647026643122_WP , & ! Dcoeffs(gC2H3CHO,gC2H2)
       0.000226721270000202_WP , & ! Dcoeffs(gNXC3H7,gC2H2)
       0.000208223516957756_WP , & ! Dcoeffs(gC4H7,gC2H2)
       0.000207040801757039_WP , & ! Dcoeffs(gPXC4H9,gC2H2)
       0.000189403303932833_WP , & ! Dcoeffs(gPXC5H11,gC2H2)
       0.000162495685945984_WP , & ! Dcoeffs(gPXC7H15,gC2H2)
       0.000127631668438687_WP , & ! Dcoeffs(gPXC12H25,gC2H2)
       0.000127631668438687_WP , & ! Dcoeffs(gS3XC12H25,gC2H2)
       0.000127631668438687_WP , & ! Dcoeffs(gSXC12H25,gC2H2)
       0.000125035637840811_WP , & ! Dcoeffs(gC12OOH,gC2H2)
       0.000116545937800143_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H2)
       0.000248195378531972_WP , & ! Dcoeffs(gN2,gC3H6)
       0.00037011520958668_WP , & ! Dcoeffs(gO,gC3H6)
       0.000869851712063682_WP , & ! Dcoeffs(gH2,gC3H6)
       0.00153551791112805_WP , & ! Dcoeffs(gH,gC3H6)
       0.00036208186123872_WP , & ! Dcoeffs(gOH,gC3H6)
       0.000368485998027465_WP , & ! Dcoeffs(gH2O,gC3H6)
       0.000243850404676122_WP , & ! Dcoeffs(gH2O2,gC3H6)
       0.000248061594032902_WP , & ! Dcoeffs(gO2,gC3H6)
       0.000245900705230394_WP , & ! Dcoeffs(gHO2,gC3H6)
       0.000244929672868588_WP , & ! Dcoeffs(gCH2O,gC3H6)
       0.000212398177327266_WP , & ! Dcoeffs(gCO2,gC3H6)
       0.000293504209433033_WP , & ! Dcoeffs(gCH3,gC3H6)
       0.00024655692454695_WP , & ! Dcoeffs(gCO,gC3H6)
       0.000208717268586092_WP , & ! Dcoeffs(gC2H6,gC3H6)
       0.000290187606194083_WP , & ! Dcoeffs(gCH4,gC3H6)
       0.000229090786719459_WP , & ! Dcoeffs(gC2H4,gC3H6)
       0.000227741855257729_WP , & ! Dcoeffs(gC2H2,gC3H6)
       0.000165433077320096_WP , & ! Dcoeffs(gC3H6,gC3H6)
       0.000148894068006559_WP , & ! Dcoeffs(gC4H81,gC3H6)
       0.000137828423249892_WP , & ! Dcoeffs(gC5H10,gC3H6)
       0.000126354190578546_WP , & ! Dcoeffs(gC6H12,gC3H6)
       0.000117740559068001_WP , & ! Dcoeffs(gC7H14,gC3H6)
       0.000110889352029114_WP , & ! Dcoeffs(gC8H16,gC3H6)
       0.000104782458091141_WP , & ! Dcoeffs(gC9H18,gC3H6)
       9.90902208309617e-05_WP , & ! Dcoeffs(gC10H20,gC3H6)
       8.74517283125637e-05_WP , & ! Dcoeffs(gC12H25O2,gC3H6)
       8.96305590556458e-05_WP , & ! Dcoeffs(gNXC12H26,gC3H6)
       8.45173169533533e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC3H6)
       0.000301174306105888_WP , & ! Dcoeffs(gCH2,gC3H6)
       0.000247399779913829_WP , & ! Dcoeffs(gHCO,gC3H6)
       0.000301174306105888_WP , & ! Dcoeffs(gCH2D,gC3H6)
       0.000237034577868547_WP , & ! Dcoeffs(gCH3O,gC3H6)
       0.000225104655716171_WP , & ! Dcoeffs(gC2H3,gC3H6)
       0.000203797681730068_WP , & ! Dcoeffs(gCH2CHO,gC3H6)
       0.000210817930277107_WP , & ! Dcoeffs(gC2H5,gC3H6)
       0.000166445057228172_WP , & ! Dcoeffs(gAXC3H5,gC3H6)
       0.000148917969028037_WP , & ! Dcoeffs(gC2H3CHO,gC3H6)
       0.000164462651454952_WP , & ! Dcoeffs(gNXC3H7,gC3H6)
       0.000149476656977362_WP , & ! Dcoeffs(gC4H7,gC3H6)
       0.000148329875417399_WP , & ! Dcoeffs(gPXC4H9,gC3H6)
       0.000134759415648903_WP , & ! Dcoeffs(gPXC5H11,gC3H6)
       0.00011483745313062_WP , & ! Dcoeffs(gPXC7H15,gC3H6)
       8.96833961226298e-05_WP , & ! Dcoeffs(gPXC12H25,gC3H6)
       8.96833961226298e-05_WP , & ! Dcoeffs(gS3XC12H25,gC3H6)
       8.96833961226298e-05_WP , & ! Dcoeffs(gSXC12H25,gC3H6)
       8.74517283125637e-05_WP , & ! Dcoeffs(gC12OOH,gC3H6)
       8.15470509198558e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC3H6)
       0.000225196754854586_WP , & ! Dcoeffs(gN2,gC4H81)
       0.000339873240031536_WP , & ! Dcoeffs(gO,gC4H81)
       0.000823914146914244_WP , & ! Dcoeffs(gH2,gC4H81)
       0.00144991627230797_WP , & ! Dcoeffs(gH,gC4H81)
       0.000331944129572735_WP , & ! Dcoeffs(gOH,gC4H81)
       0.000336955933322258_WP , & ! Dcoeffs(gH2O,gC4H81)
       0.000219609084488098_WP , & ! Dcoeffs(gH2O2,gC4H81)
       0.00022387425585662_WP , & ! Dcoeffs(gO2,gC4H81)
       0.000221686793370783_WP , & ! Dcoeffs(gHO2,gC4H81)
       0.000221682275566736_WP , & ! Dcoeffs(gCH2O,gC4H81)
       0.000189845045687207_WP , & ! Dcoeffs(gCO2,gC4H81)
       0.000271552174456435_WP , & ! Dcoeffs(gCH3,gC4H81)
       0.000223745939423743_WP , & ! Dcoeffs(gCO,gC4H81)
       0.000189540851293258_WP , & ! Dcoeffs(gC2H6,gC4H81)
       0.000267952525621614_WP , & ! Dcoeffs(gCH4,gC4H81)
       0.000208213406323774_WP , & ! Dcoeffs(gC2H4,gC4H81)
       0.00020762237654515_WP , & ! Dcoeffs(gC2H2,gC4H81)
       0.000148894068006559_WP , & ! Dcoeffs(gC3H6,gC4H81)
       0.00013273085475896_WP , & ! Dcoeffs(gC4H81,gC4H81)
       0.000121976267461226_WP , & ! Dcoeffs(gC5H10,gC4H81)
       0.000111239913827236_WP , & ! Dcoeffs(gC6H12,gC4H81)
       0.00010322169588917_WP , & ! Dcoeffs(gC7H14,gC4H81)
       9.68815135673317e-05_WP , & ! Dcoeffs(gC8H16,gC4H81)
       9.12917674362378e-05_WP , & ! Dcoeffs(gC9H18,gC4H81)
       8.61386507989418e-05_WP , & ! Dcoeffs(gC10H20,gC4H81)
       7.54451312148859e-05_WP , & ! Dcoeffs(gC12H25O2,gC4H81)
       7.76203638647189e-05_WP , & ! Dcoeffs(gNXC12H26,gC4H81)
       7.28303648462603e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC4H81)
       0.000279141709811096_WP , & ! Dcoeffs(gCH2,gC4H81)
       0.000224176280286762_WP , & ! Dcoeffs(gHCO,gC4H81)
       0.000279141709811096_WP , & ! Dcoeffs(gCH2D,gC4H81)
       0.000214404986667694_WP , & ! Dcoeffs(gCH3O,gC4H81)
       0.000204962486047544_WP , & ! Dcoeffs(gC2H3,gC4H81)
       0.000182485639398419_WP , & ! Dcoeffs(gCH2CHO,gC4H81)
       0.000191669156636248_WP , & ! Dcoeffs(gC2H5,gC4H81)
       0.000149934540012727_WP , & ! Dcoeffs(gAXC3H5,gC4H81)
       0.00013275571194363_WP , & ! Dcoeffs(gC2H3CHO,gC4H81)
       0.000147895465812373_WP , & ! Dcoeffs(gNXC3H7,gC4H81)
       0.000133336561708019_WP , & ! Dcoeffs(gC4H7,gC4H81)
       0.000132143897624127_WP , & ! Dcoeffs(gPXC4H9,gC4H81)
       0.000119244682106281_WP , & ! Dcoeffs(gPXC5H11,gC4H81)
       0.000100685100058564_WP , & ! Dcoeffs(gPXC7H15,gC4H81)
       7.76775899332576e-05_WP , & ! Dcoeffs(gPXC12H25,gC4H81)
       7.76775899332576e-05_WP , & ! Dcoeffs(gS3XC12H25,gC4H81)
       7.76775899332576e-05_WP , & ! Dcoeffs(gSXC12H25,gC4H81)
       7.54451312148859e-05_WP , & ! Dcoeffs(gC12OOH,gC4H81)
       7.01913692648357e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC4H81)
       0.000209582350691676_WP , & ! Dcoeffs(gN2,gC5H10)
       0.000318754186562301_WP , & ! Dcoeffs(gO,gC5H10)
       0.000788389618820296_WP , & ! Dcoeffs(gH2,gC5H10)
       0.00138307922069536_WP , & ! Dcoeffs(gH,gC5H10)
       0.000310968096727009_WP , & ! Dcoeffs(gOH,gC5H10)
       0.000315076567413716_WP , & ! Dcoeffs(gH2O,gC5H10)
       0.000203266001885732_WP , & ! Dcoeffs(gH2O2,gC5H10)
       0.000207532800205218_WP , & ! Dcoeffs(gO2,gC5H10)
       0.00020534532344366_WP , & ! Dcoeffs(gHO2,gC5H10)
       0.000205940958771633_WP , & ! Dcoeffs(gCH2O,gC5H10)
       0.000174756670981324_WP , & ! Dcoeffs(gCO2,gC5H10)
       0.000256187969908242_WP , & ! Dcoeffs(gCH3,gC5H10)
       0.000208259244917311_WP , & ! Dcoeffs(gCO,gC5H10)
       0.000176568515512598_WP , & ! Dcoeffs(gC2H6,gC5H10)
       0.000252445484657336_WP , & ! Dcoeffs(gCH4,gC5H10)
       0.000194046239033776_WP , & ! Dcoeffs(gC2H4,gC5H10)
       0.00019393047709591_WP , & ! Dcoeffs(gC2H2,gC5H10)
       0.000137828423249892_WP , & ! Dcoeffs(gC3H6,gC5H10)
       0.000121976267461226_WP , & ! Dcoeffs(gC4H81,gC5H10)
       0.000111454511473909_WP , & ! Dcoeffs(gC5H10,gC5H10)
       0.000101218028906945_WP , & ! Dcoeffs(gC6H12,gC5H10)
       9.35968311077393e-05_WP , & ! Dcoeffs(gC7H14,gC5H10)
       8.75937681199031e-05_WP , & ! Dcoeffs(gC8H16,gC5H10)
       8.23432881889357e-05_WP , & ! Dcoeffs(gC9H18,gC5H10)
       7.75431666095816e-05_WP , & ! Dcoeffs(gC10H20,gC5H10)
       6.74577529817375e-05_WP , & ! Dcoeffs(gC12H25O2,gC5H10)
       6.96393164155479e-05_WP , & ! Dcoeffs(gNXC12H26,gC5H10)
       6.50509953450722e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC5H10)
       0.000263659609650322_WP , & ! Dcoeffs(gCH2,gC5H10)
       0.000208430478498754_WP , & ! Dcoeffs(gHCO,gC5H10)
       0.000263659609650322_WP , & ! Dcoeffs(gCH2D,gC5H10)
       0.000199102482116461_WP , & ! Dcoeffs(gCH3O,gC5H10)
       0.000191276667932132_WP , & ! Dcoeffs(gC2H3,gC5H10)
       0.000168221499696896_WP , & ! Dcoeffs(gCH2CHO,gC5H10)
       0.000178699011529846_WP , & ! Dcoeffs(gC2H5,gC5H10)
       0.000138881520928908_WP , & ! Dcoeffs(gAXC3H5,gC5H10)
       0.000122001648439668_WP , & ! Dcoeffs(gC2H3CHO,gC5H10)
       0.000136817054580977_WP , & ! Dcoeffs(gNXC3H7,gC5H10)
       0.000122594588370442_WP , & ! Dcoeffs(gC4H7,gC5H10)
       0.000121376787688063_WP , & ! Dcoeffs(gPXC4H9,gC5H10)
       0.000108948217412536_WP , & ! Dcoeffs(gPXC5H11,gC5H10)
       9.13029983474016e-05_WP , & ! Dcoeffs(gPXC7H15,gC5H10)
       6.96997464711787e-05_WP , & ! Dcoeffs(gPXC12H25,gC5H10)
       6.96997464711787e-05_WP , & ! Dcoeffs(gS3XC12H25,gC5H10)
       6.96997464711787e-05_WP , & ! Dcoeffs(gSXC12H25,gC5H10)
       6.74577529817375e-05_WP , & ! Dcoeffs(gC12OOH,gC5H10)
       6.26276221057583e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC5H10)
       0.0001920817664437_WP , & ! Dcoeffs(gN2,gC6H12)
       0.00029272348894414_WP , & ! Dcoeffs(gO,gC6H12)
       0.000734799049707001_WP , & ! Dcoeffs(gH2,gC6H12)
       0.00128044619666764_WP , & ! Dcoeffs(gH,gC6H12)
       0.000285341835959668_WP , & ! Dcoeffs(gOH,gC6H12)
       0.000288522921049236_WP , & ! Dcoeffs(gH2O,gC6H12)
       0.000185408652242449_WP , & ! Dcoeffs(gH2O2,gC6H12)
       0.000189522231103024_WP , & ! Dcoeffs(gO2,gC6H12)
       0.000187413907348326_WP , & ! Dcoeffs(gHO2,gC6H12)
       0.000188467498229115_WP , & ! Dcoeffs(gCH2O,gC6H12)
       0.000158908394652196_WP , & ! Dcoeffs(gCO2,gC6H12)
       0.000237310697023796_WP , & ! Dcoeffs(gCH3,gC6H12)
       0.000190908434346346_WP , & ! Dcoeffs(gCO,gC6H12)
       0.000162325857215685_WP , & ! Dcoeffs(gC2H6,gC6H12)
       0.000233566290072705_WP , & ! Dcoeffs(gCH4,gC6H12)
       0.000178253391975693_WP , & ! Dcoeffs(gC2H4,gC6H12)
       0.000178525627390824_WP , & ! Dcoeffs(gC2H2,gC6H12)
       0.000126354190578546_WP , & ! Dcoeffs(gC3H6,gC6H12)
       0.000111239913827236_WP , & ! Dcoeffs(gC4H81,gC6H12)
       0.000101218028906945_WP , & ! Dcoeffs(gC5H10,gC6H12)
       9.16656493822454e-05_WP , & ! Dcoeffs(gC6H12,gC6H12)
       8.45638409137608e-05_WP , & ! Dcoeffs(gC7H14,gC6H12)
       7.89825043733598e-05_WP , & ! Dcoeffs(gC8H16,gC6H12)
       7.41300739724276e-05_WP , & ! Dcoeffs(gC9H18,gC6H12)
       6.97225159863661e-05_WP , & ! Dcoeffs(gC10H20,gC6H12)
       6.03454213138423e-05_WP , & ! Dcoeffs(gC12H25O2,gC6H12)
       6.24814460553531e-05_WP , & ! Dcoeffs(gNXC12H26,gC6H12)
       5.81529628634045e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC6H12)
       0.000244438529694406_WP , & ! Dcoeffs(gCH2,gC6H12)
       0.000190864859778323_WP , & ! Dcoeffs(gHCO,gC6H12)
       0.000244438529694406_WP , & ! Dcoeffs(gCH2D,gC6H12)
       0.000182222872938972_WP , & ! Dcoeffs(gCH3O,gC6H12)
       0.000175966338193128_WP , & ! Dcoeffs(gC2H3,gC6H12)
       0.000153244728325669_WP , & ! Dcoeffs(gCH2CHO,gC6H12)
       0.000164386970516639_WP , & ! Dcoeffs(gC2H5,gC6H12)
       0.000127383719406674_WP , & ! Dcoeffs(gAXC3H5,gC6H12)
       0.000111264912318932_WP , & ! Dcoeffs(gC2H3CHO,gC6H12)
       0.000125364963188108_WP , & ! Dcoeffs(gNXC3H7,gC6H12)
       0.000111848799005379_WP , & ! Dcoeffs(gC4H7,gC6H12)
       0.000110649346496714_WP , & ! Dcoeffs(gPXC4H9,gC6H12)
       9.8959092352701e-05_WP , & ! Dcoeffs(gPXC5H11,gC6H12)
       8.25187761144338e-05_WP , & ! Dcoeffs(gPXC7H15,gC6H12)
       6.25429190487523e-05_WP , & ! Dcoeffs(gPXC12H25,gC6H12)
       6.25429190487523e-05_WP , & ! Dcoeffs(gS3XC12H25,gC6H12)
       6.25429190487523e-05_WP , & ! Dcoeffs(gSXC12H25,gC6H12)
       6.03454213138423e-05_WP , & ! Dcoeffs(gC12OOH,gC6H12)
       5.59489322285093e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC6H12)
       0.000178900919575114_WP , & ! Dcoeffs(gN2,gC7H14)
       0.000272964594538869_WP , & ! Dcoeffs(gO,gC7H14)
       0.000692787777153306_WP , & ! Dcoeffs(gH2,gC7H14)
       0.00120055466250143_WP , & ! Dcoeffs(gH,gC7H14)
       0.00026591831706951_WP , & ! Dcoeffs(gOH,gC7H14)
       0.000268449382403498_WP , & ! Dcoeffs(gH2O,gC7H14)
       0.000172036906064542_WP , & ! Dcoeffs(gH2O2,gC7H14)
       0.000176015649720003_WP , & ! Dcoeffs(gO2,gC7H14)
       0.000173976890042496_WP , & ! Dcoeffs(gHO2,gC7H14)
       0.000175332416290554_WP , & ! Dcoeffs(gCH2O,gC7H14)
       0.000147090401095983_WP , & ! Dcoeffs(gCO2,gC7H14)
       0.000222849316681022_WP , & ! Dcoeffs(gCH3,gC7H14)
       0.000177838013537895_WP , & ! Dcoeffs(gCO,gC7H14)
       0.000151578609467526_WP , & ! Dcoeffs(gC2H6,gC7H14)
       0.000219131654013742_WP , & ! Dcoeffs(gCH4,gC7H14)
       0.000166335434384876_WP , & ! Dcoeffs(gC2H4,gC7H14)
       0.000166870147382517_WP , & ! Dcoeffs(gC2H2,gC7H14)
       0.000117740559068001_WP , & ! Dcoeffs(gC3H6,gC7H14)
       0.00010322169588917_WP , & ! Dcoeffs(gC4H81,gC7H14)
       9.35968311077393e-05_WP , & ! Dcoeffs(gC5H10,gC7H14)
       8.45638409137608e-05_WP , & ! Dcoeffs(gC6H12,gC7H14)
       7.78534437690515e-05_WP , & ! Dcoeffs(gC7H14,gC7H14)
       7.258777042506e-05_WP , & ! Dcoeffs(gC8H16,gC7H14)
       6.80310608299065e-05_WP , & ! Dcoeffs(gC9H18,gC7H14)
       6.39135784011103e-05_WP , & ! Dcoeffs(gC10H20,gC7H14)
       5.50588655178693e-05_WP , & ! Dcoeffs(gC12H25O2,gC7H14)
       5.71606180302749e-05_WP , & ! Dcoeffs(gNXC12H26,gC7H14)
       5.30233082180385e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC7H14)
       0.000229688721172769_WP , & ! Dcoeffs(gCH2,gC7H14)
       0.000177649182312746_WP , & ! Dcoeffs(gHCO,gC7H14)
       0.000229688721172769_WP , & ! Dcoeffs(gCH2D,gC7H14)
       0.000169537162862932_WP , & ! Dcoeffs(gCH3O,gC7H14)
       0.000164393751134414_WP , & ! Dcoeffs(gC2H3,gC7H14)
       0.000142058672079223_WP , & ! Dcoeffs(gCH2CHO,gC7H14)
       0.000153577976436335_WP , & ! Dcoeffs(gC2H5,gC7H14)
       0.000118747668192603_WP , & ! Dcoeffs(gAXC3H5,gC7H14)
       0.000103246298165941_WP , & ! Dcoeffs(gC2H3CHO,gC7H14)
       0.000116772487257256_WP , & ! Dcoeffs(gNXC3H7,gC7H14)
       0.000103820835768584_WP , & ! Dcoeffs(gC4H7,gC7H14)
       0.000102640390796742_WP , & ! Dcoeffs(gPXC4H9,gC7H14)
       9.15208656413486e-05_WP , & ! Dcoeffs(gPXC5H11,gC7H14)
       7.59910315920014e-05_WP , & ! Dcoeffs(gPXC7H15,gC7H14)
       5.72227985645459e-05_WP , & ! Dcoeffs(gPXC12H25,gC7H14)
       5.72227985645459e-05_WP , & ! Dcoeffs(gS3XC12H25,gC7H14)
       5.72227985645459e-05_WP , & ! Dcoeffs(gSXC12H25,gC7H14)
       5.50588655178693e-05_WP , & ! Dcoeffs(gC12OOH,gC7H14)
       5.09797533571647e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC7H14)
       0.000168365009971315_WP , & ! Dcoeffs(gN2,gC8H16)
       0.000257040437728607_WP , & ! Dcoeffs(gO,gC8H16)
       0.000657977079764113_WP , & ! Dcoeffs(gH2,gC8H16)
       0.00113474647434735_WP , & ! Dcoeffs(gH,gC8H16)
       0.000250285334970907_WP , & ! Dcoeffs(gOH,gC8H16)
       0.000252330181586298_WP , & ! Dcoeffs(gH2O,gC8H16)
       0.000161407636570571_WP , & ! Dcoeffs(gH2O2,gC8H16)
       0.000165263161838963_WP , & ! Dcoeffs(gO2,gC8H16)
       0.000163287898372443_WP , & ! Dcoeffs(gHO2,gC8H16)
       0.000164852614692269_WP , & ! Dcoeffs(gCH2O,gC8H16)
       0.000137740675445044_WP , & ! Dcoeffs(gCO2,gC8H16)
       0.000211104749051153_WP , & ! Dcoeffs(gCH3,gC8H16)
       0.000167388847996724_WP , & ! Dcoeffs(gCO,gC8H16)
       0.000142978348342039_WP , & ! Dcoeffs(gC2H6,gC8H16)
       0.000207429342225985_WP , & ! Dcoeffs(gCH4,gC8H16)
       0.000156794286214216_WP , & ! Dcoeffs(gC2H4,gC8H16)
       0.000157516514365395_WP , & ! Dcoeffs(gC2H2,gC8H16)
       0.000110889352029114_WP , & ! Dcoeffs(gC3H6,gC8H16)
       9.68815135673317e-05_WP , & ! Dcoeffs(gC4H81,gC8H16)
       8.75937681199031e-05_WP , & ! Dcoeffs(gC5H10,gC8H16)
       7.89825043733598e-05_WP , & ! Dcoeffs(gC6H12,gC8H16)
       7.258777042506e-05_WP , & ! Dcoeffs(gC7H14,gC8H16)
       6.75749539845493e-05_WP , & ! Dcoeffs(gC8H16,gC8H16)
       6.32530534383672e-05_WP , & ! Dcoeffs(gC9H18,gC8H16)
       5.93642765834865e-05_WP , & ! Dcoeffs(gC10H20,gC8H16)
       5.09220255548948e-05_WP , & ! Dcoeffs(gC12H25O2,gC8H16)
       5.29945207472548e-05_WP , & ! Dcoeffs(gNXC12H26,gC8H16)
       4.90087170417891e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC8H16)
       0.000217691407671062_WP , & ! Dcoeffs(gCH2,gC8H16)
       0.000167096155991644_WP , & ! Dcoeffs(gHCO,gC8H16)
       0.000217691407671062_WP , & ! Dcoeffs(gCH2D,gC8H16)
       0.000159419465513179_WP , & ! Dcoeffs(gCH3O,gC8H16)
       0.000155115549509499_WP , & ! Dcoeffs(gC2H3,gC8H16)
       0.00013319679394919_WP , & ! Dcoeffs(gCH2CHO,gC8H16)
       0.000144920831317879_WP , & ! Dcoeffs(gC2H5,gC8H16)
       0.000111874650502795_WP , & ! Dcoeffs(gAXC3H5,gC8H16)
       9.69057041398598e-05_WP , & ! Dcoeffs(gC2H3CHO,gC8H16)
       0.000109941935968103_WP , & ! Dcoeffs(gNXC3H7,gC8H16)
       9.7470549621138e-05_WP , & ! Dcoeffs(gC4H7,gC8H16)
       9.63098559258124e-05_WP , & ! Dcoeffs(gPXC4H9,gC8H16)
       8.56618485091308e-05_WP , & ! Dcoeffs(gPXC5H11,gC8H16)
       7.0867821742006e-05_WP , & ! Dcoeffs(gPXC7H15,gC8H16)
       5.30571311938653e-05_WP , & ! Dcoeffs(gPXC12H25,gC8H16)
       5.30571311938653e-05_WP , & ! Dcoeffs(gS3XC12H25,gC8H16)
       5.30571311938653e-05_WP , & ! Dcoeffs(gSXC12H25,gC8H16)
       5.09220255548948e-05_WP , & ! Dcoeffs(gC12OOH,gC8H16)
       4.70900193992173e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC8H16)
       0.00015884647366511_WP , & ! Dcoeffs(gN2,gC9H18)
       0.000242431210566166_WP , & ! Dcoeffs(gO,gC9H18)
       0.000624913953539292_WP , & ! Dcoeffs(gH2,gC9H18)
       0.00107252508098565_WP , & ! Dcoeffs(gH,gC9H18)
       0.000235969229292287_WP , & ! Dcoeffs(gOH,gC9H18)
       0.000237605541436434_WP , & ! Dcoeffs(gH2O,gC9H18)
       0.000151878011216899_WP , & ! Dcoeffs(gH2O2,gC9H18)
       0.00015560101232286_WP , & ! Dcoeffs(gO2,gC9H18)
       0.000153693928073407_WP , & ! Dcoeffs(gHO2,gC9H18)
       0.000155409810727048_WP , & ! Dcoeffs(gCH2O,gC9H18)
       0.000129433824316387_WP , & ! Dcoeffs(gCO2,gC9H18)
       0.000200259974075612_WP , & ! Dcoeffs(gCH3,gC9H18)
       0.000157947819571172_WP , & ! Dcoeffs(gCO,gC9H18)
       0.000135217384133035_WP , & ! Dcoeffs(gC2H6,gC9H18)
       0.000196647401127184_WP , & ! Dcoeffs(gCH4,gC9H18)
       0.00014816731624933_WP , & ! Dcoeffs(gC2H4,gC9H18)
       0.000149032954966575_WP , & ! Dcoeffs(gC2H2,gC9H18)
       0.000104782458091141_WP , & ! Dcoeffs(gC3H6,gC9H18)
       9.12917674362378e-05_WP , & ! Dcoeffs(gC4H81,gC9H18)
       8.23432881889357e-05_WP , & ! Dcoeffs(gC5H10,gC9H18)
       7.41300739724276e-05_WP , & ! Dcoeffs(gC6H12,gC9H18)
       6.80310608299065e-05_WP , & ! Dcoeffs(gC7H14,gC9H18)
       6.32530534383672e-05_WP , & ! Dcoeffs(gC8H16,gC9H18)
       5.91458857986559e-05_WP , & ! Dcoeffs(gC9H18,gC9H18)
       5.54633068693717e-05_WP , & ! Dcoeffs(gC10H20,gC9H18)
       4.73986665738814e-05_WP , & ! Dcoeffs(gC12H25O2,gC9H18)
       4.94363621656186e-05_WP , & ! Dcoeffs(gNXC12H26,gC9H18)
       4.55931982839178e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC9H18)
       0.000206590111585317_WP , & ! Dcoeffs(gCH2,gC9H18)
       0.000157575293702182_WP , & ! Dcoeffs(gHCO,gC9H18)
       0.000206590111585317_WP , & ! Dcoeffs(gCH2D,gC9H18)
       0.000150311891763921_WP , & ! Dcoeffs(gCH3O,gC9H18)
       0.000146712365348944_WP , & ! Dcoeffs(gC2H3,gC9H18)
       0.00012531392556729_WP , & ! Dcoeffs(gCH2CHO,gC9H18)
       0.000137098298991053_WP , & ! Dcoeffs(gC2H5,gC9H18)
       0.000105742457037518_WP , & ! Dcoeffs(gAXC3H5,gC9H18)
       9.13154389035168e-05_WP , & ! Dcoeffs(gC2H3CHO,gC9H18)
       0.000103859117497247_WP , & ! Dcoeffs(gNXC3H7,gC9H18)
       9.18680991673087e-05_WP , & ! Dcoeffs(gC4H7,gC9H18)
       9.07323104375586e-05_WP , & ! Dcoeffs(gPXC4H9,gC9H18)
       8.05397936078778e-05_WP , & ! Dcoeffs(gPXC5H11,gC9H18)
       6.64357717034349e-05_WP , & ! Dcoeffs(gPXC7H15,gC9H18)
       4.94989593436907e-05_WP , & ! Dcoeffs(gPXC12H25,gC9H18)
       4.94989593436907e-05_WP , & ! Dcoeffs(gS3XC12H25,gC9H18)
       4.94989593436907e-05_WP , & ! Dcoeffs(gSXC12H25,gC9H18)
       4.73986665738814e-05_WP , & ! Dcoeffs(gC12OOH,gC9H18)
       4.37842273156116e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC9H18)
       0.000149865759257987_WP , & ! Dcoeffs(gN2,gC10H20)
       0.000228466358448342_WP , & ! Dcoeffs(gO,gC10H20)
       0.000592368074973416_WP , & ! Dcoeffs(gH2,gC10H20)
       0.00101159523521699_WP , & ! Dcoeffs(gH,gC10H20)
       0.000222306365870113_WP , & ! Dcoeffs(gOH,gC10H20)
       0.000223585993350154_WP , & ! Dcoeffs(gH2O,gC10H20)
       0.000142952343913222_WP , & ! Dcoeffs(gH2O2,gC10H20)
       0.000146531617168546_WP , & ! Dcoeffs(gO2,gC10H20)
       0.000144698385783289_WP , & ! Dcoeffs(gHO2,gC10H20)
       0.000146522753837095_WP , & ! Dcoeffs(gCH2O,gC10H20)
       0.000121720008225509_WP , & ! Dcoeffs(gCO2,gC10H20)
       0.000189822700784405_WP , & ! Dcoeffs(gCH3,gC10H20)
       0.000149039288940324_WP , & ! Dcoeffs(gCO,gC10H20)
       0.000127899361856981_WP , & ! Dcoeffs(gC2H6,gC10H20)
       0.000186291240861066_WP , & ! Dcoeffs(gCH4,gC10H20)
       0.000140019478624668_WP , & ! Dcoeffs(gC2H4,gC10H20)
       0.000140996831322474_WP , & ! Dcoeffs(gC2H2,gC10H20)
       9.90902208309617e-05_WP , & ! Dcoeffs(gC3H6,gC10H20)
       8.61386507989418e-05_WP , & ! Dcoeffs(gC4H81,gC10H20)
       7.75431666095816e-05_WP , & ! Dcoeffs(gC5H10,gC10H20)
       6.97225159863661e-05_WP , & ! Dcoeffs(gC6H12,gC10H20)
       6.39135784011103e-05_WP , & ! Dcoeffs(gC7H14,gC10H20)
       5.93642765834865e-05_WP , & ! Dcoeffs(gC8H16,gC10H20)
       5.54633068693717e-05_WP , & ! Dcoeffs(gC9H18,gC10H20)
       5.19759796668878e-05_WP , & ! Dcoeffs(gC10H20,gC10H20)
       4.42757551534723e-05_WP , & ! Dcoeffs(gC12H25O2,gC10H20)
       4.62713005440182e-05_WP , & ! Dcoeffs(gNXC12H26,gC10H20)
       4.25702763253711e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC10H20)
       0.000195886463446135_WP , & ! Dcoeffs(gCH2,gC10H20)
       0.000148604124015245_WP , & ! Dcoeffs(gHCO,gC10H20)
       0.000195886463446135_WP , & ! Dcoeffs(gCH2D,gC10H20)
       0.00014174765838263_WP , & ! Dcoeffs(gCH3O,gC10H20)
       0.000138762872403942_WP , & ! Dcoeffs(gC2H3,gC10H20)
       0.000117984466500867_WP , & ! Dcoeffs(gCH2CHO,gC10H20)
       0.000129713133221413_WP , & ! Dcoeffs(gC2H5,gC10H20)
       0.000100021238267204_WP , & ! Dcoeffs(gAXC3H5,gC10H20)
       8.61616950526822e-05_WP , & ! Dcoeffs(gC2H3CHO,gC10H20)
       9.8194548751463e-05_WP , & ! Dcoeffs(gNXC3H7,gC10H20)
       8.66996581449494e-05_WP , & ! Dcoeffs(gC4H7,gC10H20)
       8.55939619551224e-05_WP , & ! Dcoeffs(gPXC4H9,gC10H20)
       7.58593877028169e-05_WP , & ! Dcoeffs(gPXC5H11,gC10H20)
       6.24323280203902e-05_WP , & ! Dcoeffs(gPXC7H15,gC10H20)
       4.6333457758663e-05_WP , & ! Dcoeffs(gPXC12H25,gC10H20)
       4.6333457758663e-05_WP , & ! Dcoeffs(gS3XC12H25,gC10H20)
       4.6333457758663e-05_WP , & ! Dcoeffs(gSXC12H25,gC10H20)
       4.42757551534723e-05_WP , & ! Dcoeffs(gC12OOH,gC10H20)
       4.08626386056184e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC10H20)
       0.000132108906772413_WP , & ! Dcoeffs(gN2,gC12H25O2)
       0.000201604361130571_WP , & ! Dcoeffs(gO,gC12H25O2)
       0.000530899051369924_WP , & ! Dcoeffs(gH2,gC12H25O2)
       0.000898099753942287_WP , & ! Dcoeffs(gH,gC12H25O2)
       0.000195991916812534_WP , & ! Dcoeffs(gOH,gC12H25O2)
       0.000196614066548498_WP , & ! Dcoeffs(gH2O,gC12H25O2)
       0.000125237271721254_WP , & ! Dcoeffs(gH2O2,gC12H25O2)
       0.000128567746090024_WP , & ! Dcoeffs(gO2,gC12H25O2)
       0.000126862575292341_WP , & ! Dcoeffs(gHO2,gC12H25O2)
       0.000128920870476707_WP , & ! Dcoeffs(gCH2O,gC12H25O2)
       0.000106193774124665_WP , & ! Dcoeffs(gCO2,gC12H25O2)
       0.000169469562465148_WP , & ! Dcoeffs(gCH3,gC12H25O2)
       0.000131417814874274_WP , & ! Dcoeffs(gCO,gC12H25O2)
       0.000113256360163701_WP , & ! Dcoeffs(gC2H6,gC12H25O2)
       0.000166081811727536_WP , & ! Dcoeffs(gCH4,gC12H25O2)
       0.000123827344024322_WP , & ! Dcoeffs(gC2H4,gC12H25O2)
       0.000125035637840811_WP , & ! Dcoeffs(gC2H2,gC12H25O2)
       8.74517283125637e-05_WP , & ! Dcoeffs(gC3H6,gC12H25O2)
       7.54451312148859e-05_WP , & ! Dcoeffs(gC4H81,gC12H25O2)
       6.74577529817375e-05_WP , & ! Dcoeffs(gC5H10,gC12H25O2)
       6.03454213138423e-05_WP , & ! Dcoeffs(gC6H12,gC12H25O2)
       5.50588655178693e-05_WP , & ! Dcoeffs(gC7H14,gC12H25O2)
       5.09220255548948e-05_WP , & ! Dcoeffs(gC8H16,gC12H25O2)
       4.73986665738814e-05_WP , & ! Dcoeffs(gC9H18,gC12H25O2)
       4.42757551534723e-05_WP , & ! Dcoeffs(gC10H20,gC12H25O2)
       3.72179581018383e-05_WP , & ! Dcoeffs(gC12H25O2,gC12H25O2)
       3.91769169035378e-05_WP , & ! Dcoeffs(gNXC12H26,gC12H25O2)
       3.57025916723498e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC12H25O2)
       0.000175044312379159_WP , & ! Dcoeffs(gCH2,gC12H25O2)
       0.000130854911935186_WP , & ! Dcoeffs(gHCO,gC12H25O2)
       0.000175044312379159_WP , & ! Dcoeffs(gCH2D,gC12H25O2)
       0.000124741567618169_WP , & ! Dcoeffs(gCH3O,gC12H25O2)
       0.000122954971223631_WP , & ! Dcoeffs(gC2H3,gC12H25O2)
       0.000103197462637561_WP , & ! Dcoeffs(gCH2CHO,gC12H25O2)
       0.000114952663045108_WP , & ! Dcoeffs(gC2H5,gC12H25O2)
       8.83349234013819e-05_WP , & ! Dcoeffs(gAXC3H5,gC12H25O2)
       7.5467229380022e-05_WP , & ! Dcoeffs(gC2H3CHO,gC12H25O2)
       8.66014735991708e-05_WP , & ! Dcoeffs(gNXC3H7,gC12H25O2)
       7.59829480247145e-05_WP , & ! Dcoeffs(gC4H7,gC12H25O2)
       7.49226403238765e-05_WP , & ! Dcoeffs(gPXC4H9,gC12H25O2)
       6.60051819681908e-05_WP , & ! Dcoeffs(gPXC5H11,gC12H25O2)
       5.3803938046675e-05_WP , & ! Dcoeffs(gPXC7H15,gC12H25O2)
       3.92400355888598e-05_WP , & ! Dcoeffs(gPXC12H25,gC12H25O2)
       3.92400355888598e-05_WP , & ! Dcoeffs(gS3XC12H25,gC12H25O2)
       3.92400355888598e-05_WP , & ! Dcoeffs(gSXC12H25,gC12H25O2)
       3.72179581018383e-05_WP , & ! Dcoeffs(gC12OOH,gC12H25O2)
       3.41888617550563e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC12H25O2)
       0.000134950078900224_WP , & ! Dcoeffs(gN2,gNXC12H26)
       0.00020523534853619_WP , & ! Dcoeffs(gO,gNXC12H26)
       0.000537258398625781_WP , & ! Dcoeffs(gH2,gNXC12H26)
       0.000909404221351503_WP , & ! Dcoeffs(gH,gNXC12H26)
       0.000199598416371206_WP , & ! Dcoeffs(gOH,gNXC12H26)
       0.00020034171685952_WP , & ! Dcoeffs(gH2O,gNXC12H26)
       0.00012820545955085_WP , & ! Dcoeffs(gH2O2,gNXC12H26)
       0.000131528601098381_WP , & ! Dcoeffs(gO2,gNXC12H26)
       0.000129826912395245_WP , & ! Dcoeffs(gHO2,gNXC12H26)
       0.000131786601791341_WP , & ! Dcoeffs(gCH2O,gNXC12H26)
       0.00010901388678168_WP , & ! Dcoeffs(gCO2,gNXC12H26)
       0.00017227137081761_WP , & ! Dcoeffs(gCH3,gNXC12H26)
       0.000134239933465597_WP , & ! Dcoeffs(gCO,gNXC12H26)
       0.000115700802420398_WP , & ! Dcoeffs(gC2H6,gNXC12H26)
       0.000168901576001868_WP , & ! Dcoeffs(gCH4,gNXC12H26)
       0.000126450572878396_WP , & ! Dcoeffs(gC2H4,gNXC12H26)
       0.000127581325785075_WP , & ! Dcoeffs(gC2H2,gNXC12H26)
       8.96305590556458e-05_WP , & ! Dcoeffs(gC3H6,gNXC12H26)
       7.76203638647189e-05_WP , & ! Dcoeffs(gC4H81,gNXC12H26)
       6.96393164155479e-05_WP , & ! Dcoeffs(gC5H10,gNXC12H26)
       6.24814460553531e-05_WP , & ! Dcoeffs(gC6H12,gNXC12H26)
       5.71606180302749e-05_WP , & ! Dcoeffs(gC7H14,gNXC12H26)
       5.29945207472548e-05_WP , & ! Dcoeffs(gC8H16,gNXC12H26)
       4.94363621656186e-05_WP , & ! Dcoeffs(gC9H18,gNXC12H26)
       4.62713005440182e-05_WP , & ! Dcoeffs(gC10H20,gNXC12H26)
       3.91769169035378e-05_WP , & ! Dcoeffs(gC12H25O2,gNXC12H26)
       4.10965886084227e-05_WP , & ! Dcoeffs(gNXC12H26,gNXC12H26)
       3.76345179971392e-05_WP , & ! Dcoeffs(gOC12H23OOH,gNXC12H26)
       0.000177868644093084_WP , & ! Dcoeffs(gCH2,gNXC12H26)
       0.000133718352035274_WP , & ! Dcoeffs(gHCO,gNXC12H26)
       0.000177868644093084_WP , & ! Dcoeffs(gCH2D,gNXC12H26)
       0.000127544954560424_WP , & ! Dcoeffs(gCH3O,gNXC12H26)
       0.00012550199067687_WP , & ! Dcoeffs(gC2H3,gNXC12H26)
       0.000105887577652325_WP , & ! Dcoeffs(gCH2CHO,gNXC12H26)
       0.000117393978455242_WP , & ! Dcoeffs(gC2H5,gNXC12H26)
       9.05082870093224e-05_WP , & ! Dcoeffs(gAXC3H5,gNXC12H26)
       7.76422320856617e-05_WP , & ! Dcoeffs(gC2H3CHO,gNXC12H26)
       8.87858228590416e-05_WP , & ! Dcoeffs(gNXC3H7,gNXC12H26)
       7.81526532715637e-05_WP , & ! Dcoeffs(gC4H7,gNXC12H26)
       7.71033814311622e-05_WP , & ! Dcoeffs(gPXC4H9,gNXC12H26)
       6.81510461321594e-05_WP , & ! Dcoeffs(gPXC5H11,gNXC12H26)
       5.58639541524881e-05_WP , & ! Dcoeffs(gPXC7H15,gNXC12H26)
       4.1157707466652e-05_WP , & ! Dcoeffs(gPXC12H25,gNXC12H26)
       4.1157707466652e-05_WP , & ! Dcoeffs(gS3XC12H25,gNXC12H26)
       4.1157707466652e-05_WP , & ! Dcoeffs(gSXC12H25,gNXC12H26)
       3.91769169035378e-05_WP , & ! Dcoeffs(gC12OOH,gNXC12H26)
       3.60918501787869e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gNXC12H26)
       0.000127451043084698_WP , & ! Dcoeffs(gN2,gOC12H23OOH)
       0.000194286397852819_WP , & ! Dcoeffs(gO,gOC12H23OOH)
       0.000513059144249488_WP , & ! Dcoeffs(gH2,gOC12H23OOH)
       0.000865307046269039_WP , & ! Dcoeffs(gH,gOC12H23OOH)
       0.000188849537741492_WP , & ! Dcoeffs(gOH,gOC12H23OOH)
       0.000189324530809433_WP , & ! Dcoeffs(gH2O,gOC12H23OOH)
       0.000120668726728134_WP , & ! Dcoeffs(gH2O2,gOC12H23OOH)
       0.000123909755949014_WP , & ! Dcoeffs(gO2,gOC12H23OOH)
       0.000122250484954982_WP , & ! Dcoeffs(gHO2,gOC12H23OOH)
       0.000124331001766918_WP , & ! Dcoeffs(gCH2O,gOC12H23OOH)
       0.000102286517995063_WP , & ! Dcoeffs(gCO2,gOC12H23OOH)
       0.000163879696584055_WP , & ! Dcoeffs(gCH3,gOC12H23OOH)
       0.000126795223114576_WP , & ! Dcoeffs(gCO,gOC12H23OOH)
       0.000109438588193007_WP , & ! Dcoeffs(gC2H6,gOC12H23OOH)
       0.000160554869189162_WP , & ! Dcoeffs(gCH4,gOC12H23OOH)
       0.000119579587322224_WP , & ! Dcoeffs(gC2H4,gOC12H23OOH)
       0.000120821587251503_WP , & ! Dcoeffs(gC2H2,gOC12H23OOH)
       8.45173169533533e-05_WP , & ! Dcoeffs(gC3H6,gOC12H23OOH)
       7.28303648462603e-05_WP , & ! Dcoeffs(gC4H81,gOC12H23OOH)
       6.50509953450722e-05_WP , & ! Dcoeffs(gC5H10,gOC12H23OOH)
       5.81529628634045e-05_WP , & ! Dcoeffs(gC6H12,gOC12H23OOH)
       5.30233082180385e-05_WP , & ! Dcoeffs(gC7H14,gOC12H23OOH)
       4.90087170417891e-05_WP , & ! Dcoeffs(gC8H16,gOC12H23OOH)
       4.55931982839178e-05_WP , & ! Dcoeffs(gC9H18,gOC12H23OOH)
       4.25702763253711e-05_WP , & ! Dcoeffs(gC10H20,gOC12H23OOH)
       3.57025916723498e-05_WP , & ! Dcoeffs(gC12H25O2,gOC12H23OOH)
       3.76345179971392e-05_WP , & ! Dcoeffs(gNXC12H26,gOC12H23OOH)
       3.42365392260067e-05_WP , & ! Dcoeffs(gOC12H23OOH,gOC12H23OOH)
       0.000169296237079779_WP , & ! Dcoeffs(gCH2,gOC12H23OOH)
       0.000126212990976249_WP , & ! Dcoeffs(gHCO,gOC12H23OOH)
       0.000169296237079779_WP , & ! Dcoeffs(gCH2D,gOC12H23OOH)
       0.000120319707140167_WP , & ! Dcoeffs(gCH3O,gOC12H23OOH)
       0.000118794822726399_WP , & ! Dcoeffs(gC2H3,gOC12H23OOH)
       9.94693874775008e-05_WP , & ! Dcoeffs(gCH2CHO,gOC12H23OOH)
       0.000111092495765152_WP , & ! Dcoeffs(gC2H5,gOC12H23OOH)
       8.53811754680519e-05_WP , & ! Dcoeffs(gAXC3H5,gOC12H23OOH)
       7.28520242561716e-05_WP , & ! Dcoeffs(gC2H3CHO,gOC12H23OOH)
       8.36855776892108e-05_WP , & ! Dcoeffs(gNXC3H7,gOC12H23OOH)
       7.33574759716593e-05_WP , & ! Dcoeffs(gC4H7,gOC12H23OOH)
       7.23182193814527e-05_WP , & ! Dcoeffs(gPXC4H9,gOC12H23OOH)
       6.36582530382576e-05_WP , & ! Dcoeffs(gPXC5H11,gOC12H23OOH)
       5.18237969674593e-05_WP , & ! Dcoeffs(gPXC7H15,gOC12H23OOH)
       3.76971391844953e-05_WP , & ! Dcoeffs(gPXC12H25,gOC12H23OOH)
       3.76971391844953e-05_WP , & ! Dcoeffs(gS3XC12H25,gOC12H23OOH)
       3.76971391844953e-05_WP , & ! Dcoeffs(gSXC12H25,gOC12H23OOH)
       3.57025916723498e-05_WP , & ! Dcoeffs(gC12OOH,gOC12H23OOH)
       3.27724685244368e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gOC12H23OOH)
       0.000447444145472319_WP , & ! Dcoeffs(gN2,gCH2)
       0.000642305387033784_WP , & ! Dcoeffs(gO,gCH2)
       0.0012565531454962_WP , & ! Dcoeffs(gH2,gCH2)
       0.00227002526260045_WP , & ! Dcoeffs(gH,gCH2)
       0.00063335188868657_WP , & ! Dcoeffs(gOH,gCH2)
       0.000653924740850306_WP , & ! Dcoeffs(gH2O,gCH2)
       0.000453806180071497_WP , & ! Dcoeffs(gH2O2,gCH2)
       0.000457960599613033_WP , & ! Dcoeffs(gO2,gCH2)
       0.000455824684474729_WP , & ! Dcoeffs(gHO2,gCH2)
       0.00044614970894825_WP , & ! Dcoeffs(gCH2O,gCH2)
       0.000403850166800589_WP , & ! Dcoeffs(gCO2,gCH2)
       0.000484193286057581_WP , & ! Dcoeffs(gCH3,gCH2)
       0.000443993904098287_WP , & ! Dcoeffs(gCO,gCH2)
       0.000371097831056395_WP , & ! Dcoeffs(gC2H6,gCH2)
       0.000483642913476372_WP , & ! Dcoeffs(gCH4,gCH2)
       0.00040796905944982_WP , & ! Dcoeffs(gC2H4,gCH2)
       0.000399816225585138_WP , & ! Dcoeffs(gC2H2,gCH2)
       0.000301174306105888_WP , & ! Dcoeffs(gC3H6,gCH2)
       0.000279141709811096_WP , & ! Dcoeffs(gC4H81,gCH2)
       0.000263659609650322_WP , & ! Dcoeffs(gC5H10,gCH2)
       0.000244438529694406_WP , & ! Dcoeffs(gC6H12,gCH2)
       0.000229688721172769_WP , & ! Dcoeffs(gC7H14,gCH2)
       0.000217691407671062_WP , & ! Dcoeffs(gC8H16,gCH2)
       0.000206590111585317_WP , & ! Dcoeffs(gC9H18,gCH2)
       0.000195886463446135_WP , & ! Dcoeffs(gC10H20,gCH2)
       0.000175044312379159_WP , & ! Dcoeffs(gC12H25O2,gCH2)
       0.000177868644093084_WP , & ! Dcoeffs(gNXC12H26,gCH2)
       0.000169296237079779_WP , & ! Dcoeffs(gOC12H23OOH,gCH2)
       0.000492519272064652_WP , & ! Dcoeffs(gCH2,gCH2)
       0.0004486101671241_WP , & ! Dcoeffs(gHCO,gCH2)
       0.000492519272064652_WP , & ! Dcoeffs(gCH2D,gCH2)
       0.000432064411947566_WP , & ! Dcoeffs(gCH3O,gCH2)
       0.000397198976615491_WP , & ! Dcoeffs(gC2H3,gCH2)
       0.000383655124051852_WP , & ! Dcoeffs(gCH2CHO,gCH2)
       0.000373139496802049_WP , & ! Dcoeffs(gC2H5,gCH2)
       0.000302096875611003_WP , & ! Dcoeffs(gAXC3H5,gCH2)
       0.000279162621502943_WP , & ! Dcoeffs(gC2H3CHO,gCH2)
       0.000300292265242635_WP , & ! Dcoeffs(gNXC3H7,gCH2)
       0.000279651942715328_WP , & ! Dcoeffs(gC4H7,gCH2)
       0.000278648602251554_WP , & ! Dcoeffs(gPXC4H9,gCH2)
       0.000257512778460287_WP , & ! Dcoeffs(gPXC5H11,gCH2)
       0.000223588142923537_WP , & ! Dcoeffs(gPXC7H15,gCH2)
       0.000177908920783083_WP , & ! Dcoeffs(gPXC12H25,gCH2)
       0.000177908920783083_WP , & ! Dcoeffs(gS3XC12H25,gCH2)
       0.000177908920783083_WP , & ! Dcoeffs(gSXC12H25,gCH2)
       0.000175044312379159_WP , & ! Dcoeffs(gC12OOH,gCH2)
       0.00016344308285273_WP , & ! Dcoeffs(gO2C12H24OOH,gCH2)
       0.000383729937200005_WP , & ! Dcoeffs(gN2,gHCO)
       0.000583610619857961_WP , & ! Dcoeffs(gO,gHCO)
       0.00129473304243188_WP , & ! Dcoeffs(gH2,gHCO)
       0.00239954478974831_WP , & ! Dcoeffs(gH,gHCO)
       0.000572354460797411_WP , & ! Dcoeffs(gOH,gHCO)
       0.000588793143710801_WP , & ! Dcoeffs(gH2O,gHCO)
       0.000383250399475299_WP , & ! Dcoeffs(gH2O2,gHCO)
       0.000388768258423406_WP , & ! Dcoeffs(gO2,gHCO)
       0.000385934928915501_WP , & ! Dcoeffs(gHO2,gHCO)
       0.000380416067601609_WP , & ! Dcoeffs(gCH2O,gHCO)
       0.00033320267702311_WP , & ! Dcoeffs(gCO2,gHCO)
       0.000438354289090043_WP , & ! Dcoeffs(gCH3,gHCO)
       0.00038069658269363_WP , & ! Dcoeffs(gCO,gHCO)
       0.000314763572537127_WP , & ! Dcoeffs(gC2H6,gHCO)
       0.000435528206761748_WP , & ! Dcoeffs(gCH4,gHCO)
       0.000348925042271393_WP , & ! Dcoeffs(gC2H4,gHCO)
       0.000343892910678475_WP , & ! Dcoeffs(gC2H2,gHCO)
       0.000247399779913829_WP , & ! Dcoeffs(gC3H6,gHCO)
       0.000224176280286762_WP , & ! Dcoeffs(gC4H81,gHCO)
       0.000208430478498754_WP , & ! Dcoeffs(gC5H10,gHCO)
       0.000190864859778323_WP , & ! Dcoeffs(gC6H12,gHCO)
       0.000177649182312746_WP , & ! Dcoeffs(gC7H14,gHCO)
       0.000167096155991644_WP , & ! Dcoeffs(gC8H16,gHCO)
       0.000157575293702182_WP , & ! Dcoeffs(gC9H18,gHCO)
       0.000148604124015245_WP , & ! Dcoeffs(gC10H20,gHCO)
       0.000130854911935186_WP , & ! Dcoeffs(gC12H25O2,gHCO)
       0.000133718352035274_WP , & ! Dcoeffs(gNXC12H26,gHCO)
       0.000126212990976249_WP , & ! Dcoeffs(gOC12H23OOH,gHCO)
       0.0004486101671241_WP , & ! Dcoeffs(gCH2,gHCO)
       0.000383649559682664_WP , & ! Dcoeffs(gHCO,gHCO)
       0.0004486101671241_WP , & ! Dcoeffs(gCH2D,gHCO)
       0.000367071525077352_WP , & ! Dcoeffs(gCH3O,gHCO)
       0.000340498140908928_WP , & ! Dcoeffs(gC2H3,gHCO)
       0.000316607973529085_WP , & ! Dcoeffs(gCH2CHO,gHCO)
       0.000317433284500904_WP , & ! Dcoeffs(gC2H5,gHCO)
       0.000248635852235381_WP , & ! Dcoeffs(gAXC3H5,gHCO)
       0.000224204904812347_WP , & ! Dcoeffs(gC2H3CHO,gHCO)
       0.000246215766964114_WP , & ! Dcoeffs(gNXC3H7,gHCO)
       0.000224874271896314_WP , & ! Dcoeffs(gC4H7,gHCO)
       0.00022350086137314_WP , & ! Dcoeffs(gPXC4H9,gHCO)
       0.000203282475682939_WP , & ! Dcoeffs(gPXC5H11,gHCO)
       0.000172739947812579_WP , & ! Dcoeffs(gPXC7H15,gHCO)
       0.000133776278788218_WP , & ! Dcoeffs(gPXC12H25,gHCO)
       0.000133776278788218_WP , & ! Dcoeffs(gS3XC12H25,gHCO)
       0.000133776278788218_WP , & ! Dcoeffs(gSXC12H25,gHCO)
       0.000130854911935186_WP , & ! Dcoeffs(gC12OOH,gHCO)
       0.000121516290789697_WP , & ! Dcoeffs(gO2C12H24OOH,gHCO)
       0.000447444145472319_WP , & ! Dcoeffs(gN2,gCH2D)
       0.000642305387033784_WP , & ! Dcoeffs(gO,gCH2D)
       0.0012565531454962_WP , & ! Dcoeffs(gH2,gCH2D)
       0.00227002526260045_WP , & ! Dcoeffs(gH,gCH2D)
       0.00063335188868657_WP , & ! Dcoeffs(gOH,gCH2D)
       0.000653924740850306_WP , & ! Dcoeffs(gH2O,gCH2D)
       0.000453806180071497_WP , & ! Dcoeffs(gH2O2,gCH2D)
       0.000457960599613033_WP , & ! Dcoeffs(gO2,gCH2D)
       0.000455824684474729_WP , & ! Dcoeffs(gHO2,gCH2D)
       0.00044614970894825_WP , & ! Dcoeffs(gCH2O,gCH2D)
       0.000403850166800589_WP , & ! Dcoeffs(gCO2,gCH2D)
       0.000484193286057581_WP , & ! Dcoeffs(gCH3,gCH2D)
       0.000443993904098287_WP , & ! Dcoeffs(gCO,gCH2D)
       0.000371097831056395_WP , & ! Dcoeffs(gC2H6,gCH2D)
       0.000483642913476372_WP , & ! Dcoeffs(gCH4,gCH2D)
       0.00040796905944982_WP , & ! Dcoeffs(gC2H4,gCH2D)
       0.000399816225585138_WP , & ! Dcoeffs(gC2H2,gCH2D)
       0.000301174306105888_WP , & ! Dcoeffs(gC3H6,gCH2D)
       0.000279141709811096_WP , & ! Dcoeffs(gC4H81,gCH2D)
       0.000263659609650322_WP , & ! Dcoeffs(gC5H10,gCH2D)
       0.000244438529694406_WP , & ! Dcoeffs(gC6H12,gCH2D)
       0.000229688721172769_WP , & ! Dcoeffs(gC7H14,gCH2D)
       0.000217691407671062_WP , & ! Dcoeffs(gC8H16,gCH2D)
       0.000206590111585317_WP , & ! Dcoeffs(gC9H18,gCH2D)
       0.000195886463446135_WP , & ! Dcoeffs(gC10H20,gCH2D)
       0.000175044312379159_WP , & ! Dcoeffs(gC12H25O2,gCH2D)
       0.000177868644093084_WP , & ! Dcoeffs(gNXC12H26,gCH2D)
       0.000169296237079779_WP , & ! Dcoeffs(gOC12H23OOH,gCH2D)
       0.000492519272064652_WP , & ! Dcoeffs(gCH2,gCH2D)
       0.0004486101671241_WP , & ! Dcoeffs(gHCO,gCH2D)
       0.000492519272064652_WP , & ! Dcoeffs(gCH2D,gCH2D)
       0.000432064411947566_WP , & ! Dcoeffs(gCH3O,gCH2D)
       0.000397198976615491_WP , & ! Dcoeffs(gC2H3,gCH2D)
       0.000383655124051852_WP , & ! Dcoeffs(gCH2CHO,gCH2D)
       0.000373139496802049_WP , & ! Dcoeffs(gC2H5,gCH2D)
       0.000302096875611003_WP , & ! Dcoeffs(gAXC3H5,gCH2D)
       0.000279162621502943_WP , & ! Dcoeffs(gC2H3CHO,gCH2D)
       0.000300292265242635_WP , & ! Dcoeffs(gNXC3H7,gCH2D)
       0.000279651942715328_WP , & ! Dcoeffs(gC4H7,gCH2D)
       0.000278648602251554_WP , & ! Dcoeffs(gPXC4H9,gCH2D)
       0.000257512778460287_WP , & ! Dcoeffs(gPXC5H11,gCH2D)
       0.000223588142923537_WP , & ! Dcoeffs(gPXC7H15,gCH2D)
       0.000177908920783083_WP , & ! Dcoeffs(gPXC12H25,gCH2D)
       0.000177908920783083_WP , & ! Dcoeffs(gS3XC12H25,gCH2D)
       0.000177908920783083_WP , & ! Dcoeffs(gSXC12H25,gCH2D)
       0.000175044312379159_WP , & ! Dcoeffs(gC12OOH,gCH2D)
       0.00016344308285273_WP , & ! Dcoeffs(gO2C12H24OOH,gCH2D)
       0.000367299622176243_WP , & ! Dcoeffs(gN2,gCH3O)
       0.000559059055422749_WP , & ! Dcoeffs(gO,gCH3O)
       0.00125320178294477_WP , & ! Dcoeffs(gH2,gCH3O)
       0.00231413777814519_WP , & ! Dcoeffs(gH,gCH3O)
       0.000548019025603849_WP , & ! Dcoeffs(gOH,gCH3O)
       0.000563095822515933_WP , & ! Dcoeffs(gH2O,gCH3O)
       0.000366012903512566_WP , & ! Dcoeffs(gH2O2,gCH3O)
       0.000371472622572972_WP , & ! Dcoeffs(gO2,gCH3O)
       0.000368669495958801_WP , & ! Dcoeffs(gHO2,gCH3O)
       0.000363873439761536_WP , & ! Dcoeffs(gCH2O,gCH3O)
       0.000317909507080548_WP , & ! Dcoeffs(gCO2,gCH3O)
       0.000421970631469897_WP , & ! Dcoeffs(gCH3,gCH3O)
       0.000364437170891124_WP , & ! Dcoeffs(gCO,gCH3O)
       0.00030181995272476_WP , & ! Dcoeffs(gC2H6,gCH3O)
       0.000418962559358755_WP , & ! Dcoeffs(gCH4,gCH3O)
       0.000334405113922974_WP , & ! Dcoeffs(gC2H4,gCH3O)
       0.000329932687416168_WP , & ! Dcoeffs(gC2H2,gCH3O)
       0.000237034577868547_WP , & ! Dcoeffs(gC3H6,gCH3O)
       0.000214404986667694_WP , & ! Dcoeffs(gC4H81,gCH3O)
       0.000199102482116461_WP , & ! Dcoeffs(gC5H10,gCH3O)
       0.000182222872938972_WP , & ! Dcoeffs(gC6H12,gCH3O)
       0.000169537162862932_WP , & ! Dcoeffs(gC7H14,gCH3O)
       0.000159419465513179_WP , & ! Dcoeffs(gC8H16,gCH3O)
       0.000150311891763921_WP , & ! Dcoeffs(gC9H18,gCH3O)
       0.00014174765838263_WP , & ! Dcoeffs(gC10H20,gCH3O)
       0.000124741567618169_WP , & ! Dcoeffs(gC12H25O2,gCH3O)
       0.000127544954560424_WP , & ! Dcoeffs(gNXC12H26,gCH3O)
       0.000120319707140167_WP , & ! Dcoeffs(gOC12H23OOH,gCH3O)
       0.000432064411947566_WP , & ! Dcoeffs(gCH2,gCH3O)
       0.000367071525077352_WP , & ! Dcoeffs(gHCO,gCH3O)
       0.000432064411947566_WP , & ! Dcoeffs(gCH2D,gCH3O)
       0.000351144385831914_WP , & ! Dcoeffs(gCH3O,gCH3O)
       0.000326571967129171_WP , & ! Dcoeffs(gC2H3,gCH3O)
       0.000302352962786597_WP , & ! Dcoeffs(gCH2CHO,gCH3O)
       0.000304467019574378_WP , & ! Dcoeffs(gC2H5,gCH3O)
       0.000238266093422696_WP , & ! Dcoeffs(gAXC3H5,gCH3O)
       0.00021443358803475_WP , & ! Dcoeffs(gC2H3CHO,gCH3O)
       0.000235854699093722_WP , & ! Dcoeffs(gNXC3H7,gCH3O)
       0.000215102367152717_WP , & ! Dcoeffs(gC4H7,gCH3O)
       0.000213730066651336_WP , & ! Dcoeffs(gPXC4H9,gCH3O)
       0.000194214420564705_WP , & ! Dcoeffs(gPXC5H11,gCH3O)
       0.000164887978894275_WP , & ! Dcoeffs(gPXC7H15,gCH3O)
       0.000127603453260587_WP , & ! Dcoeffs(gPXC12H25,gCH3O)
       0.000127603453260587_WP , & ! Dcoeffs(gS3XC12H25,gCH3O)
       0.000127603453260587_WP , & ! Dcoeffs(gSXC12H25,gCH3O)
       0.000124741567618169_WP , & ! Dcoeffs(gC12OOH,gCH3O)
       0.000115846898719432_WP , & ! Dcoeffs(gO2C12H24OOH,gCH3O)
       0.000340658772501297_WP , & ! Dcoeffs(gN2,gC2H3)
       0.000506386415172215_WP , & ! Dcoeffs(gO,gC2H3)
       0.00111607964131565_WP , & ! Dcoeffs(gH2,gC2H3)
       0.00202054388179126_WP , & ! Dcoeffs(gH,gC2H3)
       0.000496869018696843_WP , & ! Dcoeffs(gOH,gC2H3)
       0.000509607383896679_WP , & ! Dcoeffs(gH2O,gC2H3)
       0.000339773788557372_WP , & ! Dcoeffs(gH2O2,gC2H3)
       0.000344481569164019_WP , & ! Dcoeffs(gO2,gC2H3)
       0.000342063887251606_WP , & ! Dcoeffs(gHO2,gC2H3)
       0.000337729802341268_WP , & ! Dcoeffs(gCH2O,gC2H3)
       0.000297720609563858_WP , & ! Dcoeffs(gCO2,gC2H3)
       0.000388331802303509_WP , & ! Dcoeffs(gCH3,gC2H3)
       0.000338143737917509_WP , & ! Dcoeffs(gCO,gC2H3)
       0.000282821830348657_WP , & ! Dcoeffs(gC2H6,gC2H3)
       0.000385663993734222_WP , & ! Dcoeffs(gCH4,gC2H3)
       0.000311666622462626_WP , & ! Dcoeffs(gC2H4,gC2H3)
       0.00030762206998283_WP , & ! Dcoeffs(gC2H2,gC2H3)
       0.000225104655716171_WP , & ! Dcoeffs(gC3H6,gC2H3)
       0.000204962486047544_WP , & ! Dcoeffs(gC4H81,gC2H3)
       0.000191276667932132_WP , & ! Dcoeffs(gC5H10,gC2H3)
       0.000175966338193128_WP , & ! Dcoeffs(gC6H12,gC2H3)
       0.000164393751134414_WP , & ! Dcoeffs(gC7H14,gC2H3)
       0.000155115549509499_WP , & ! Dcoeffs(gC8H16,gC2H3)
       0.000146712365348944_WP , & ! Dcoeffs(gC9H18,gC2H3)
       0.000138762872403942_WP , & ! Dcoeffs(gC10H20,gC2H3)
       0.000122954971223631_WP , & ! Dcoeffs(gC12H25O2,gC2H3)
       0.00012550199067687_WP , & ! Dcoeffs(gNXC12H26,gC2H3)
       0.000118794822726399_WP , & ! Dcoeffs(gOC12H23OOH,gC2H3)
       0.000397198976615491_WP , & ! Dcoeffs(gCH2,gC2H3)
       0.000340498140908928_WP , & ! Dcoeffs(gHCO,gC2H3)
       0.000397198976615491_WP , & ! Dcoeffs(gCH2D,gC2H3)
       0.000326571967129171_WP , & ! Dcoeffs(gCH3O,gC2H3)
       0.000304687166744395_WP , & ! Dcoeffs(gC2H3,gC2H3)
       0.000283847662511271_WP , & ! Dcoeffs(gCH2CHO,gC2H3)
       0.000285135061137414_WP , & ! Dcoeffs(gC2H5,gC2H3)
       0.000226182872490482_WP , & ! Dcoeffs(gAXC3H5,gC2H3)
       0.000204987456002222_WP , & ! Dcoeffs(gC2H3CHO,gC2H3)
       0.000224072059077806_WP , & ! Dcoeffs(gNXC3H7,gC2H3)
       0.00020557140476922_WP , & ! Dcoeffs(gC4H7,gC2H3)
       0.000204373342284973_WP , & ! Dcoeffs(gPXC4H9,gC2H3)
       0.000186801395186717_WP , & ! Dcoeffs(gPXC5H11,gC2H3)
       0.000160079021540353_WP , & ! Dcoeffs(gPXC7H15,gC2H3)
       0.000125553167077626_WP , & ! Dcoeffs(gPXC12H25,gC2H3)
       0.000125553167077626_WP , & ! Dcoeffs(gS3XC12H25,gC2H3)
       0.000125553167077626_WP , & ! Dcoeffs(gSXC12H25,gC2H3)
       0.000122954971223631_WP , & ! Dcoeffs(gC12OOH,gC2H3)
       0.000114575332534957_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H3)
       0.000317350215668815_WP , & ! Dcoeffs(gN2,gCH2CHO)
       0.000488467158662646_WP , & ! Dcoeffs(gO,gCH2CHO)
       0.00114355723934641_WP , & ! Dcoeffs(gH2,gCH2CHO)
       0.00209462165090589_WP , & ! Dcoeffs(gH,gCH2CHO)
       0.000477798288594454_WP , & ! Dcoeffs(gOH,gCH2CHO)
       0.000488994214287629_WP , & ! Dcoeffs(gH2O,gCH2CHO)
       0.000313238483230046_WP , & ! Dcoeffs(gH2O2,gCH2CHO)
       0.00031870232346589_WP , & ! Dcoeffs(gO2,gCH2CHO)
       0.000315898777976519_WP , & ! Dcoeffs(gHO2,gCH2CHO)
       0.000313417501445691_WP , & ! Dcoeffs(gCH2O,gCH2CHO)
       0.000270065235384694_WP , & ! Dcoeffs(gCO2,gCH2CHO)
       0.000373828611114409_WP , & ! Dcoeffs(gCH3,gCH2CHO)
       0.000314973336046677_WP , & ! Dcoeffs(gCO,gCH2CHO)
       0.000261677891312613_WP , & ! Dcoeffs(gC2H6,gCH2CHO)
       0.000370147331429965_WP , & ! Dcoeffs(gCH4,gCH2CHO)
       0.000289892045039247_WP , & ! Dcoeffs(gC2H4,gCH2CHO)
       0.000287202347347334_WP , & ! Dcoeffs(gC2H2,gCH2CHO)
       0.000203797681730068_WP , & ! Dcoeffs(gC3H6,gCH2CHO)
       0.000182485639398419_WP , & ! Dcoeffs(gC4H81,gCH2CHO)
       0.000168221499696896_WP , & ! Dcoeffs(gC5H10,gCH2CHO)
       0.000153244728325669_WP , & ! Dcoeffs(gC6H12,gCH2CHO)
       0.000142058672079223_WP , & ! Dcoeffs(gC7H14,gCH2CHO)
       0.00013319679394919_WP , & ! Dcoeffs(gC8H16,gCH2CHO)
       0.00012531392556729_WP , & ! Dcoeffs(gC9H18,gCH2CHO)
       0.000117984466500867_WP , & ! Dcoeffs(gC10H20,gCH2CHO)
       0.000103197462637561_WP , & ! Dcoeffs(gC12H25O2,gCH2CHO)
       0.000105887577652325_WP , & ! Dcoeffs(gNXC12H26,gCH2CHO)
       9.94693874775008e-05_WP , & ! Dcoeffs(gOC12H23OOH,gCH2CHO)
       0.000383655124051852_WP , & ! Dcoeffs(gCH2,gCH2CHO)
       0.000316607973529085_WP , & ! Dcoeffs(gHCO,gCH2CHO)
       0.000383655124051852_WP , & ! Dcoeffs(gCH2D,gCH2CHO)
       0.000302352962786597_WP , & ! Dcoeffs(gCH3O,gCH2CHO)
       0.000283847662511271_WP , & ! Dcoeffs(gC2H3,gCH2CHO)
       0.000257584671109484_WP , & ! Dcoeffs(gCH2CHO,gCH2CHO)
       0.000264336322746727_WP , & ! Dcoeffs(gC2H5,gCH2CHO)
       0.000205058448049653_WP , & ! Dcoeffs(gAXC3H5,gCH2CHO)
       0.000182515313170543_WP , & ! Dcoeffs(gC2H3CHO,gCH2CHO)
       0.00020258860296109_WP , & ! Dcoeffs(gNXC3H7,gCH2CHO)
       0.000183208921990029_WP , & ! Dcoeffs(gC4H7,gCH2CHO)
       0.000181785160825414_WP , & ! Dcoeffs(gPXC4H9,gCH2CHO)
       0.000164116924643614_WP , & ! Dcoeffs(gPXC5H11,gCH2CHO)
       0.000138221327771118_WP , & ! Dcoeffs(gPXC7H15,gCH2CHO)
       0.000105951141788741_WP , & ! Dcoeffs(gPXC12H25,gCH2CHO)
       0.000105951141788741_WP , & ! Dcoeffs(gS3XC12H25,gCH2CHO)
       0.000105951141788741_WP , & ! Dcoeffs(gSXC12H25,gCH2CHO)
       0.000103197462637561_WP , & ! Dcoeffs(gC12OOH,gCH2CHO)
       9.57075762085096e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gCH2CHO)
       0.000317748224750734_WP , & ! Dcoeffs(gN2,gC2H5)
       0.00047159101604939_WP , & ! Dcoeffs(gO,gC2H5)
       0.00105197847173935_WP , & ! Dcoeffs(gH2,gC2H5)
       0.00189171433063692_WP , & ! Dcoeffs(gH,gC2H5)
       0.000462490670226356_WP , & ! Dcoeffs(gOH,gC2H5)
       0.000473528591229947_WP , & ! Dcoeffs(gH2O,gC2H5)
       0.00031602507004796_WP , & ! Dcoeffs(gH2O2,gC2H5)
       0.00032057857659548_WP , & ! Dcoeffs(gO2,gC2H5)
       0.000318240432387707_WP , & ! Dcoeffs(gHO2,gC2H5)
       0.000314755936351615_WP , & ! Dcoeffs(gCH2O,gC2H5)
       0.000276846893012916_WP , & ! Dcoeffs(gCO2,gC2H5)
       0.000364604921198876_WP , & ! Dcoeffs(gCH3,gC2H5)
       0.000315463536043639_WP , & ! Dcoeffs(gCO,gC2H5)
       0.000264726884369387_WP , & ! Dcoeffs(gC2H6,gC2H5)
       0.00036178223453537_WP , & ! Dcoeffs(gCH4,gC2H5)
       0.000291346797495705_WP , & ! Dcoeffs(gC2H4,gC2H5)
       0.000287979834044314_WP , & ! Dcoeffs(gC2H2,gC2H5)
       0.000210817930277107_WP , & ! Dcoeffs(gC3H6,gC2H5)
       0.000191669156636248_WP , & ! Dcoeffs(gC4H81,gC2H5)
       0.000178699011529846_WP , & ! Dcoeffs(gC5H10,gC2H5)
       0.000164386970516639_WP , & ! Dcoeffs(gC6H12,gC2H5)
       0.000153577976436335_WP , & ! Dcoeffs(gC7H14,gC2H5)
       0.000144920831317879_WP , & ! Dcoeffs(gC8H16,gC2H5)
       0.000137098298991053_WP , & ! Dcoeffs(gC9H18,gC2H5)
       0.000129713133221413_WP , & ! Dcoeffs(gC10H20,gC2H5)
       0.000114952663045108_WP , & ! Dcoeffs(gC12H25O2,gC2H5)
       0.000117393978455242_WP , & ! Dcoeffs(gNXC12H26,gC2H5)
       0.000111092495765152_WP , & ! Dcoeffs(gOC12H23OOH,gC2H5)
       0.000373139496802049_WP , & ! Dcoeffs(gCH2,gC2H5)
       0.000317433284500904_WP , & ! Dcoeffs(gHCO,gC2H5)
       0.000373139496802049_WP , & ! Dcoeffs(gCH2D,gC2H5)
       0.000304467019574378_WP , & ! Dcoeffs(gCH3O,gC2H5)
       0.000285135061137414_WP , & ! Dcoeffs(gC2H3,gC2H5)
       0.000264336322746727_WP , & ! Dcoeffs(gCH2CHO,gC2H5)
       0.000266973848711391_WP , & ! Dcoeffs(gC2H5,gC2H5)
       0.000211872129868366_WP , & ! Dcoeffs(gAXC3H5,gC2H5)
       0.000191693653740235_WP , & ! Dcoeffs(gC2H3CHO,gC2H5)
       0.000209808125938422_WP , & ! Dcoeffs(gNXC3H7,gC2H5)
       0.000192266502738242_WP , & ! Dcoeffs(gC4H7,gC2H5)
       0.000191091126717182_WP , & ! Dcoeffs(gPXC4H9,gC2H5)
       0.00017457871668958_WP , & ! Dcoeffs(gPXC5H11,gC2H5)
       0.000149612883587209_WP , & ! Dcoeffs(gPXC7H15,gC2H5)
       0.000117444896359053_WP , & ! Dcoeffs(gPXC12H25,gC2H5)
       0.000117444896359053_WP , & ! Dcoeffs(gS3XC12H25,gC2H5)
       0.000117444896359053_WP , & ! Dcoeffs(gSXC12H25,gC2H5)
       0.000114952663045108_WP , & ! Dcoeffs(gC12OOH,gC2H5)
       0.000107177377752011_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H5)
       0.00024940988755713_WP , & ! Dcoeffs(gN2,gAXC3H5)
       0.000371364371770769_WP , & ! Dcoeffs(gO,gAXC3H5)
       0.00087033962271906_WP , & ! Dcoeffs(gH2,gAXC3H5)
       0.00153595869200654_WP , & ! Dcoeffs(gH,gAXC3H5)
       0.000363358641690825_WP , & ! Dcoeffs(gOH,gAXC3H5)
       0.000369839183693048_WP , & ! Dcoeffs(gH2O,gAXC3H5)
       0.000245184465994154_WP , & ! Dcoeffs(gH2O2,gAXC3H5)
       0.000249373127939388_WP , & ! Dcoeffs(gO2,gAXC3H5)
       0.000247223703024073_WP , & ! Dcoeffs(gHO2,gAXC3H5)
       0.000246178148028855_WP , & ! Dcoeffs(gCH2O,gAXC3H5)
       0.000213726517927594_WP , & ! Dcoeffs(gCO2,gAXC3H5)
       0.000294450811767392_WP , & ! Dcoeffs(gCH3,gAXC3H5)
       0.000247763158154071_WP , & ! Dcoeffs(gCO,gAXC3H5)
       0.000209782024717738_WP , & ! Dcoeffs(gC2H6,gAXC3H5)
       0.000291168865851034_WP , & ! Dcoeffs(gCH4,gAXC3H5)
       0.000230212576404801_WP , & ! Dcoeffs(gC2H4,gAXC3H5)
       0.00022880764501909_WP , & ! Dcoeffs(gC2H2,gAXC3H5)
       0.000166445057228172_WP , & ! Dcoeffs(gC3H6,gAXC3H5)
       0.000149934540012727_WP , & ! Dcoeffs(gC4H81,gAXC3H5)
       0.000138881520928908_WP , & ! Dcoeffs(gC5H10,gAXC3H5)
       0.000127383719406674_WP , & ! Dcoeffs(gC6H12,gAXC3H5)
       0.000118747668192603_WP , & ! Dcoeffs(gC7H14,gAXC3H5)
       0.000111874650502795_WP , & ! Dcoeffs(gC8H16,gAXC3H5)
       0.000105742457037518_WP , & ! Dcoeffs(gC9H18,gAXC3H5)
       0.000100021238267204_WP , & ! Dcoeffs(gC10H20,gAXC3H5)
       8.83349234013819e-05_WP , & ! Dcoeffs(gC12H25O2,gAXC3H5)
       9.05082870093224e-05_WP , & ! Dcoeffs(gNXC12H26,gAXC3H5)
       8.53811754680519e-05_WP , & ! Dcoeffs(gOC12H23OOH,gAXC3H5)
       0.000302096875611003_WP , & ! Dcoeffs(gCH2,gAXC3H5)
       0.000248635852235381_WP , & ! Dcoeffs(gHCO,gAXC3H5)
       0.000302096875611003_WP , & ! Dcoeffs(gCH2D,gAXC3H5)
       0.000238266093422696_WP , & ! Dcoeffs(gCH3O,gAXC3H5)
       0.000226182872490482_WP , & ! Dcoeffs(gC2H3,gAXC3H5)
       0.000205058448049653_WP , & ! Dcoeffs(gCH2CHO,gAXC3H5)
       0.000211872129868366_WP , & ! Dcoeffs(gC2H5,gAXC3H5)
       0.00016745092140619_WP , & ! Dcoeffs(gAXC3H5,gAXC3H5)
       0.000149958275199209_WP , & ! Dcoeffs(gC2H3CHO,gAXC3H5)
       0.000165480566012102_WP , & ! Dcoeffs(gNXC3H7,gAXC3H5)
       0.000150513101696876_WP , & ! Dcoeffs(gC4H7,gAXC3H5)
       0.000149374277379431_WP , & ! Dcoeffs(gPXC4H9,gAXC3H5)
       0.000135794543557614_WP , & ! Dcoeffs(gPXC5H11,gAXC3H5)
       0.000115822721206297_WP , & ! Dcoeffs(gPXC7H15,gAXC3H5)
       9.05606119724398e-05_WP , & ! Dcoeffs(gPXC12H25,gAXC3H5)
       9.05606119724398e-05_WP , & ! Dcoeffs(gS3XC12H25,gAXC3H5)
       9.05606119724398e-05_WP , & ! Dcoeffs(gSXC12H25,gAXC3H5)
       8.83349234013819e-05_WP , & ! Dcoeffs(gC12OOH,gAXC3H5)
       8.23905120236073e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gAXC3H5)
       0.000225224850173203_WP , & ! Dcoeffs(gN2,gC2H3CHO)
       0.000339901489498369_WP , & ! Dcoeffs(gO,gC2H3CHO)
       0.000823924852113822_WP , & ! Dcoeffs(gH2,gC2H3CHO)
       0.00144992585804165_WP , & ! Dcoeffs(gH,gC2H3CHO)
       0.000331973053773158_WP , & ! Dcoeffs(gOH,gC2H3CHO)
       0.000336986611319825_WP , & ! Dcoeffs(gH2O,gC2H3CHO)
       0.000219640132398049_WP , & ! Dcoeffs(gH2O2,gC2H3CHO)
       0.000223904712334259_WP , & ! Dcoeffs(gO2,gC2H3CHO)
       0.000221717550331841_WP , & ! Dcoeffs(gHO2,gC2H3CHO)
       0.000221711222086638_WP , & ! Dcoeffs(gCH2O,gC2H3CHO)
       0.000189876304377523_WP , & ! Dcoeffs(gCO2,gC2H3CHO)
       0.000271573670557643_WP , & ! Dcoeffs(gCH3,gC2H3CHO)
       0.000223773847095675_WP , & ! Dcoeffs(gCO,gC2H3CHO)
       0.000189565623433162_WP , & ! Dcoeffs(gC2H6,gC2H3CHO)
       0.000267974842668811_WP , & ! Dcoeffs(gCH4,gC2H3CHO)
       0.000208239402593315_WP , & ! Dcoeffs(gC2H4,gC2H3CHO)
       0.000207647026643122_WP , & ! Dcoeffs(gC2H2,gC2H3CHO)
       0.000148917969028037_WP , & ! Dcoeffs(gC3H6,gC2H3CHO)
       0.00013275571194363_WP , & ! Dcoeffs(gC4H81,gC2H3CHO)
       0.000122001648439668_WP , & ! Dcoeffs(gC5H10,gC2H3CHO)
       0.000111264912318932_WP , & ! Dcoeffs(gC6H12,gC2H3CHO)
       0.000103246298165941_WP , & ! Dcoeffs(gC7H14,gC2H3CHO)
       9.69057041398598e-05_WP , & ! Dcoeffs(gC8H16,gC2H3CHO)
       9.13154389035168e-05_WP , & ! Dcoeffs(gC9H18,gC2H3CHO)
       8.61616950526822e-05_WP , & ! Dcoeffs(gC10H20,gC2H3CHO)
       7.5467229380022e-05_WP , & ! Dcoeffs(gC12H25O2,gC2H3CHO)
       7.76422320856617e-05_WP , & ! Dcoeffs(gNXC12H26,gC2H3CHO)
       7.28520242561716e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC2H3CHO)
       0.000279162621502943_WP , & ! Dcoeffs(gCH2,gC2H3CHO)
       0.000224204904812347_WP , & ! Dcoeffs(gHCO,gC2H3CHO)
       0.000279162621502943_WP , & ! Dcoeffs(gCH2D,gC2H3CHO)
       0.00021443358803475_WP , & ! Dcoeffs(gCH3O,gC2H3CHO)
       0.000204987456002222_WP , & ! Dcoeffs(gC2H3,gC2H3CHO)
       0.000182515313170543_WP , & ! Dcoeffs(gCH2CHO,gC2H3CHO)
       0.000191693653740235_WP , & ! Dcoeffs(gC2H5,gC2H3CHO)
       0.000149958275199209_WP , & ! Dcoeffs(gAXC3H5,gC2H3CHO)
       0.000132780564474911_WP , & ! Dcoeffs(gC2H3CHO,gC2H3CHO)
       0.000147919528189325_WP , & ! Dcoeffs(gNXC3H7,gC2H3CHO)
       0.000133361305995151_WP , & ! Dcoeffs(gC4H7,gC2H3CHO)
       0.000132168865198698_WP , & ! Dcoeffs(gPXC4H9,gC2H3CHO)
       0.000119269651926986_WP , & ! Dcoeffs(gPXC5H11,gC2H3CHO)
       0.000100709186751871_WP , & ! Dcoeffs(gPXC7H15,gC2H3CHO)
       7.76994420481365e-05_WP , & ! Dcoeffs(gPXC12H25,gC2H3CHO)
       7.76994420481365e-05_WP , & ! Dcoeffs(gS3XC12H25,gC2H3CHO)
       7.76994420481365e-05_WP , & ! Dcoeffs(gSXC12H25,gC2H3CHO)
       7.5467229380022e-05_WP , & ! Dcoeffs(gC12OOH,gC2H3CHO)
       7.02125619648556e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC2H3CHO)
       0.000247032138144801_WP , & ! Dcoeffs(gN2,gNXC3H7)
       0.000368920558473409_WP , & ! Dcoeffs(gO,gNXC3H7)
       0.000869386375913374_WP , & ! Dcoeffs(gH2,gNXC3H7)
       0.00153509763662944_WP , & ! Dcoeffs(gH,gNXC3H7)
       0.000360860616214953_WP , & ! Dcoeffs(gOH,gNXC3H7)
       0.000367191485877216_WP , & ! Dcoeffs(gH2O,gNXC3H7)
       0.000242571934406077_WP , & ! Dcoeffs(gH2O2,gNXC3H7)
       0.000246804939040818_WP , & ! Dcoeffs(gO2,gNXC3H7)
       0.000244632950220536_WP , & ! Dcoeffs(gHO2,gNXC3H7)
       0.000243733660892026_WP , & ! Dcoeffs(gCH2O,gNXC3H7)
       0.000211124209957998_WP , & ! Dcoeffs(gCO2,gNXC3H7)
       0.000292599047949749_WP , & ! Dcoeffs(gCH3,gNXC3H7)
       0.000245401611429231_WP , & ! Dcoeffs(gCO,gNXC3H7)
       0.00020769725128058_WP , & ! Dcoeffs(gC2H6,gNXC3H7)
       0.000289249160962092_WP , & ! Dcoeffs(gCH4,gNXC3H7)
       0.000228016348141257_WP , & ! Dcoeffs(gC2H4,gNXC3H7)
       0.000226721270000202_WP , & ! Dcoeffs(gC2H2,gNXC3H7)
       0.000164462651454952_WP , & ! Dcoeffs(gC3H6,gNXC3H7)
       0.000147895465812373_WP , & ! Dcoeffs(gC4H81,gNXC3H7)
       0.000136817054580977_WP , & ! Dcoeffs(gC5H10,gNXC3H7)
       0.000125364963188108_WP , & ! Dcoeffs(gC6H12,gNXC3H7)
       0.000116772487257256_WP , & ! Dcoeffs(gC7H14,gNXC3H7)
       0.000109941935968103_WP , & ! Dcoeffs(gC8H16,gNXC3H7)
       0.000103859117497247_WP , & ! Dcoeffs(gC9H18,gNXC3H7)
       9.8194548751463e-05_WP , & ! Dcoeffs(gC10H20,gNXC3H7)
       8.66014735991708e-05_WP , & ! Dcoeffs(gC12H25O2,gNXC3H7)
       8.87858228590416e-05_WP , & ! Dcoeffs(gNXC12H26,gNXC3H7)
       8.36855776892108e-05_WP , & ! Dcoeffs(gOC12H23OOH,gNXC3H7)
       0.000300292265242635_WP , & ! Dcoeffs(gCH2,gNXC3H7)
       0.000246215766964114_WP , & ! Dcoeffs(gHCO,gNXC3H7)
       0.000300292265242635_WP , & ! Dcoeffs(gCH2D,gNXC3H7)
       0.000235854699093722_WP , & ! Dcoeffs(gCH3O,gNXC3H7)
       0.000224072059077806_WP , & ! Dcoeffs(gC2H3,gNXC3H7)
       0.00020258860296109_WP , & ! Dcoeffs(gCH2CHO,gNXC3H7)
       0.000209808125938422_WP , & ! Dcoeffs(gC2H5,gNXC3H7)
       0.000165480566012102_WP , & ! Dcoeffs(gAXC3H5,gNXC3H7)
       0.000147919528189325_WP , & ! Dcoeffs(gC2H3CHO,gNXC3H7)
       0.000163486465420198_WP , & ! Dcoeffs(gNXC3H7,gNXC3H7)
       0.000148481972984753_WP , & ! Dcoeffs(gC4H7,gNXC3H7)
       0.000147327449111352_WP , & ! Dcoeffs(gPXC4H9,gNXC3H7)
       0.0001337652649349_WP , & ! Dcoeffs(gPXC5H11,gNXC3H7)
       0.000113890351484803_WP , & ! Dcoeffs(gPXC7H15,gNXC3H7)
       8.88391623340632e-05_WP , & ! Dcoeffs(gPXC12H25,gNXC3H7)
       8.88391623340632e-05_WP , & ! Dcoeffs(gS3XC12H25,gNXC3H7)
       8.88391623340632e-05_WP , & ! Dcoeffs(gSXC12H25,gNXC3H7)
       8.66014735991708e-05_WP , & ! Dcoeffs(gC12OOH,gNXC3H7)
       8.07348528249231e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gNXC3H7)
       0.000225881865460659_WP , & ! Dcoeffs(gN2,gC4H7)
       0.000340562444284468_WP , & ! Dcoeffs(gO,gC4H7)
       0.000824175536610004_WP , & ! Dcoeffs(gH2,gC4H7)
       0.0014501503454037_WP , & ! Dcoeffs(gH,gC4H7)
       0.000332649762250445_WP , & ! Dcoeffs(gOH,gC4H7)
       0.000337704317362786_WP , & ! Dcoeffs(gH2O,gC4H7)
       0.000220366048084357_WP , & ! Dcoeffs(gH2O2,gC4H7)
       0.000224616846193628_WP , & ! Dcoeffs(gO2,gC4H7)
       0.000222436686528183_WP , & ! Dcoeffs(gHO2,gC4H7)
       0.000222388095045878_WP , & ! Dcoeffs(gCH2O,gC4H7)
       0.000190606941691428_WP , & ! Dcoeffs(gCO2,gC4H7)
       0.000272076640600057_WP , & ! Dcoeffs(gCH3,gC4H7)
       0.000224426474459157_WP , & ! Dcoeffs(gCO,gC4H7)
       0.000190144883650643_WP , & ! Dcoeffs(gC2H6,gC4H7)
       0.00026849699513202_WP , & ! Dcoeffs(gCH4,gC4H7)
       0.000208847330448471_WP , & ! Dcoeffs(gC2H4,gC4H7)
       0.000208223516957756_WP , & ! Dcoeffs(gC2H2,gC4H7)
       0.000149476656977362_WP , & ! Dcoeffs(gC3H6,gC4H7)
       0.000133336561708019_WP , & ! Dcoeffs(gC4H81,gC4H7)
       0.000122594588370442_WP , & ! Dcoeffs(gC5H10,gC4H7)
       0.000111848799005379_WP , & ! Dcoeffs(gC6H12,gC4H7)
       0.000103820835768584_WP , & ! Dcoeffs(gC7H14,gC4H7)
       9.7470549621138e-05_WP , & ! Dcoeffs(gC8H16,gC4H7)
       9.18680991673087e-05_WP , & ! Dcoeffs(gC9H18,gC4H7)
       8.66996581449494e-05_WP , & ! Dcoeffs(gC10H20,gC4H7)
       7.59829480247145e-05_WP , & ! Dcoeffs(gC12H25O2,gC4H7)
       7.81526532715637e-05_WP , & ! Dcoeffs(gNXC12H26,gC4H7)
       7.33574759716593e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC4H7)
       0.000279651942715328_WP , & ! Dcoeffs(gCH2,gC4H7)
       0.000224874271896314_WP , & ! Dcoeffs(gHCO,gC4H7)
       0.000279651942715328_WP , & ! Dcoeffs(gCH2D,gC4H7)
       0.000215102367152717_WP , & ! Dcoeffs(gCH3O,gC4H7)
       0.00020557140476922_WP , & ! Dcoeffs(gC2H3,gC4H7)
       0.000183208921990029_WP , & ! Dcoeffs(gCH2CHO,gC4H7)
       0.000192266502738242_WP , & ! Dcoeffs(gC2H5,gC4H7)
       0.000150513101696876_WP , & ! Dcoeffs(gAXC3H5,gC4H7)
       0.000133361305995151_WP , & ! Dcoeffs(gC2H3CHO,gC4H7)
       0.000148481972984753_WP , & ! Dcoeffs(gNXC3H7,gC4H7)
       0.000133939529531757_WP , & ! Dcoeffs(gC4H7,gC4H7)
       0.000132752282701157_WP , & ! Dcoeffs(gPXC4H9,gC4H7)
       0.000119852977239959_WP , & ! Dcoeffs(gPXC5H11,gC4H7)
       0.000101271677902101_WP , & ! Dcoeffs(gPXC7H15,gC4H7)
       7.82094898636555e-05_WP , & ! Dcoeffs(gPXC12H25,gC4H7)
       7.82094898636555e-05_WP , & ! Dcoeffs(gS3XC12H25,gC4H7)
       7.82094898636555e-05_WP , & ! Dcoeffs(gSXC12H25,gC4H7)
       7.59829480247145e-05_WP , & ! Dcoeffs(gC12OOH,gC4H7)
       7.07070953010208e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC4H7)
       0.00022453384695368_WP , & ! Dcoeffs(gN2,gPXC4H9)
       0.000339207036985596_WP , & ! Dcoeffs(gO,gPXC4H9)
       0.000823661905417747_WP , & ! Dcoeffs(gH2,gPXC4H9)
       0.00144969042595602_WP , & ! Dcoeffs(gH,gPXC4H9)
       0.000331261980626835_WP , & ! Dcoeffs(gOH,gPXC4H9)
       0.000336232387952244_WP , & ! Dcoeffs(gH2O,gPXC4H9)
       0.000218876360104553_WP , & ! Dcoeffs(gH2O2,gPXC4H9)
       0.000223155536472373_WP , & ! Dcoeffs(gO2,gPXC4H9)
       0.000220960958945099_WP , & ! Dcoeffs(gHO2,gPXC4H9)
       0.000220999234595445_WP , & ! Dcoeffs(gCH2O,gPXC4H9)
       0.00018910713492654_WP , & ! Dcoeffs(gCO2,gPXC4H9)
       0.000271045259749179_WP , & ! Dcoeffs(gCH3,gPXC4H9)
       0.0002230874592799_WP , & ! Dcoeffs(gCO,gPXC4H9)
       0.000188956310858792_WP , & ! Dcoeffs(gC2H6,gPXC4H9)
       0.000267426224860914_WP , & ! Dcoeffs(gCH4,gPXC4H9)
       0.000207600024681994_WP , & ! Dcoeffs(gC2H4,gPXC4H9)
       0.000207040801757039_WP , & ! Dcoeffs(gC2H2,gPXC4H9)
       0.000148329875417399_WP , & ! Dcoeffs(gC3H6,gPXC4H9)
       0.000132143897624127_WP , & ! Dcoeffs(gC4H81,gPXC4H9)
       0.000121376787688063_WP , & ! Dcoeffs(gC5H10,gPXC4H9)
       0.000110649346496714_WP , & ! Dcoeffs(gC6H12,gPXC4H9)
       0.000102640390796742_WP , & ! Dcoeffs(gC7H14,gPXC4H9)
       9.63098559258124e-05_WP , & ! Dcoeffs(gC8H16,gPXC4H9)
       9.07323104375586e-05_WP , & ! Dcoeffs(gC9H18,gPXC4H9)
       8.55939619551224e-05_WP , & ! Dcoeffs(gC10H20,gPXC4H9)
       7.49226403238765e-05_WP , & ! Dcoeffs(gC12H25O2,gPXC4H9)
       7.71033814311622e-05_WP , & ! Dcoeffs(gNXC12H26,gPXC4H9)
       7.23182193814527e-05_WP , & ! Dcoeffs(gOC12H23OOH,gPXC4H9)
       0.000278648602251554_WP , & ! Dcoeffs(gCH2,gPXC4H9)
       0.00022350086137314_WP , & ! Dcoeffs(gHCO,gPXC4H9)
       0.000278648602251554_WP , & ! Dcoeffs(gCH2D,gPXC4H9)
       0.000213730066651336_WP , & ! Dcoeffs(gCH3O,gPXC4H9)
       0.000204373342284973_WP , & ! Dcoeffs(gC2H3,gPXC4H9)
       0.000181785160825414_WP , & ! Dcoeffs(gCH2CHO,gPXC4H9)
       0.000191091126717182_WP , & ! Dcoeffs(gC2H5,gPXC4H9)
       0.000149374277379431_WP , & ! Dcoeffs(gAXC3H5,gPXC4H9)
       0.000132168865198698_WP , & ! Dcoeffs(gC2H3CHO,gPXC4H9)
       0.000147327449111352_WP , & ! Dcoeffs(gNXC3H7,gPXC4H9)
       0.000132752282701157_WP , & ! Dcoeffs(gC4H7,gPXC4H9)
       0.000131554321683278_WP , & ! Dcoeffs(gPXC4H9,gPXC4H9)
       0.000118654903952813_WP , & ! Dcoeffs(gPXC5H11,gPXC4H9)
       0.000100115970995278_WP , & ! Dcoeffs(gPXC7H15,gPXC4H9)
       7.71609909181268e-05_WP , & ! Dcoeffs(gPXC12H25,gPXC4H9)
       7.71609909181268e-05_WP , & ! Dcoeffs(gS3XC12H25,gPXC4H9)
       7.71609909181268e-05_WP , & ! Dcoeffs(gSXC12H25,gPXC4H9)
       7.49226403238765e-05_WP , & ! Dcoeffs(gC12OOH,gPXC4H9)
       6.96902311602927e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gPXC4H9)
       0.000204432477443442_WP , & ! Dcoeffs(gN2,gPXC5H11)
       0.000310382619526077_WP , & ! Dcoeffs(gO,gPXC5H11)
       0.000768941661727173_WP , & ! Dcoeffs(gH2,gPXC5H11)
       0.00134519144192546_WP , & ! Dcoeffs(gH,gPXC5H11)
       0.000302780758454001_WP , & ! Dcoeffs(gOH,gPXC5H11)
       0.000306620351098606_WP , & ! Dcoeffs(gH2O,gPXC5H11)
       0.000198129908071996_WP , & ! Dcoeffs(gH2O2,gPXC5H11)
       0.000202308031455757_WP , & ! Dcoeffs(gO2,gPXC5H11)
       0.000200166065415723_WP , & ! Dcoeffs(gHO2,gPXC5H11)
       0.000200844271802073_WP , & ! Dcoeffs(gCH2O,gPXC5H11)
       0.000170401495286731_WP , & ! Dcoeffs(gCO2,gPXC5H11)
       0.00025019780185268_WP , & ! Dcoeffs(gCH3,gPXC5H11)
       0.00020315687566181_WP , & ! Dcoeffs(gCO,gPXC5H11)
       0.000172488614859723_WP , & ! Dcoeffs(gC2H6,gPXC5H11)
       0.0002464933183948_WP , & ! Dcoeffs(gCH4,gPXC5H11)
       0.000189439543158629_WP , & ! Dcoeffs(gC2H4,gPXC5H11)
       0.000189403303932833_WP , & ! Dcoeffs(gC2H2,gPXC5H11)
       0.000134759415648903_WP , & ! Dcoeffs(gC3H6,gPXC5H11)
       0.000119244682106281_WP , & ! Dcoeffs(gC4H81,gPXC5H11)
       0.000108948217412536_WP , & ! Dcoeffs(gC5H10,gPXC5H11)
       9.8959092352701e-05_WP , & ! Dcoeffs(gC6H12,gPXC5H11)
       9.15208656413486e-05_WP , & ! Dcoeffs(gC7H14,gPXC5H11)
       8.56618485091308e-05_WP , & ! Dcoeffs(gC8H16,gPXC5H11)
       8.05397936078778e-05_WP , & ! Dcoeffs(gC9H18,gPXC5H11)
       7.58593877028169e-05_WP , & ! Dcoeffs(gC10H20,gPXC5H11)
       6.60051819681908e-05_WP , & ! Dcoeffs(gC12H25O2,gPXC5H11)
       6.81510461321594e-05_WP , & ! Dcoeffs(gNXC12H26,gPXC5H11)
       6.36582530382576e-05_WP , & ! Dcoeffs(gOC12H23OOH,gPXC5H11)
       0.000257512778460287_WP , & ! Dcoeffs(gCH2,gPXC5H11)
       0.000203282475682939_WP , & ! Dcoeffs(gHCO,gPXC5H11)
       0.000257512778460287_WP , & ! Dcoeffs(gCH2D,gPXC5H11)
       0.000194214420564705_WP , & ! Dcoeffs(gCH3O,gPXC5H11)
       0.000186801395186717_WP , & ! Dcoeffs(gC2H3,gPXC5H11)
       0.000164116924643614_WP , & ! Dcoeffs(gCH2CHO,gPXC5H11)
       0.00017457871668958_WP , & ! Dcoeffs(gC2H5,gPXC5H11)
       0.000135794543557614_WP , & ! Dcoeffs(gAXC3H5,gPXC5H11)
       0.000119269651926986_WP , & ! Dcoeffs(gC2H3CHO,gPXC5H11)
       0.0001337652649349_WP , & ! Dcoeffs(gNXC3H7,gPXC5H11)
       0.000119852977239959_WP , & ! Dcoeffs(gC4H7,gPXC5H11)
       0.000118654903952813_WP , & ! Dcoeffs(gPXC4H9,gPXC5H11)
       0.000106514993621795_WP , & ! Dcoeffs(gPXC5H11,gPXC5H11)
       8.92946573258037e-05_WP , & ! Dcoeffs(gPXC7H15,gPXC5H11)
       6.82107840618616e-05_WP , & ! Dcoeffs(gPXC12H25,gPXC5H11)
       6.82107840618616e-05_WP , & ! Dcoeffs(gS3XC12H25,gPXC5H11)
       6.82107840618616e-05_WP , & ! Dcoeffs(gSXC12H25,gPXC5H11)
       6.60051819681908e-05_WP , & ! Dcoeffs(gC12OOH,gPXC5H11)
       6.12950132778709e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gPXC5H11)
       0.000173977654787646_WP , & ! Dcoeffs(gN2,gPXC7H15)
       0.000264851661077313_WP , & ! Dcoeffs(gO,gPXC7H15)
       0.000672987706321774_WP , & ! Dcoeffs(gH2,gPXC7H15)
       0.00116263768996465_WP , & ! Dcoeffs(gH,gPXC7H15)
       0.000258004930930048_WP , & ! Dcoeffs(gOH,gPXC7H15)
       0.000260320702685372_WP , & ! Dcoeffs(gH2O,gPXC7H15)
       0.000167194802475741_WP , & ! Dcoeffs(gH2O2,gPXC7H15)
       0.000171071584233537_WP , & ! Dcoeffs(gO2,gPXC7H15)
       0.000169085099529598_WP , & ! Dcoeffs(gHO2,gPXC7H15)
       0.0001704819349292_WP , & ! Dcoeffs(gCH2O,gPXC7H15)
       0.000143034840843528_WP , & ! Dcoeffs(gCO2,gPXC7H15)
       0.000216921796240202_WP , & ! Dcoeffs(gCH3,gPXC7H15)
       0.000172958057907887_WP , & ! Dcoeffs(gCO,gPXC7H15)
       0.000147660575481868_WP , & ! Dcoeffs(gC2H6,gPXC7H15)
       0.00021326345965475_WP , & ! Dcoeffs(gCH4,gPXC7H15)
       0.000161910833074417_WP , & ! Dcoeffs(gC2H4,gPXC7H15)
       0.000162495685945984_WP , & ! Dcoeffs(gC2H2,gPXC7H15)
       0.00011483745313062_WP , & ! Dcoeffs(gC3H6,gPXC7H15)
       0.000100685100058564_WP , & ! Dcoeffs(gC4H81,gPXC7H15)
       9.13029983474016e-05_WP , & ! Dcoeffs(gC5H10,gPXC7H15)
       8.25187761144338e-05_WP , & ! Dcoeffs(gC6H12,gPXC7H15)
       7.59910315920014e-05_WP , & ! Dcoeffs(gC7H14,gPXC7H15)
       7.0867821742006e-05_WP , & ! Dcoeffs(gC8H16,gPXC7H15)
       6.64357717034349e-05_WP , & ! Dcoeffs(gC9H18,gPXC7H15)
       6.24323280203902e-05_WP , & ! Dcoeffs(gC10H20,gPXC7H15)
       5.3803938046675e-05_WP , & ! Dcoeffs(gC12H25O2,gPXC7H15)
       5.58639541524881e-05_WP , & ! Dcoeffs(gNXC12H26,gPXC7H15)
       5.18237969674593e-05_WP , & ! Dcoeffs(gOC12H23OOH,gPXC7H15)
       0.000223588142923537_WP , & ! Dcoeffs(gCH2,gPXC7H15)
       0.000172739947812579_WP , & ! Dcoeffs(gHCO,gPXC7H15)
       0.000223588142923537_WP , & ! Dcoeffs(gCH2D,gPXC7H15)
       0.000164887978894275_WP , & ! Dcoeffs(gCH3O,gPXC7H15)
       0.000160079021540353_WP , & ! Dcoeffs(gC2H3,gPXC7H15)
       0.000138221327771118_WP , & ! Dcoeffs(gCH2CHO,gPXC7H15)
       0.000149612883587209_WP , & ! Dcoeffs(gC2H5,gPXC7H15)
       0.000115822721206297_WP , & ! Dcoeffs(gAXC3H5,gPXC7H15)
       0.000100709186751871_WP , & ! Dcoeffs(gC2H3CHO,gPXC7H15)
       0.000113890351484803_WP , & ! Dcoeffs(gNXC3H7,gPXC7H15)
       0.000101271677902101_WP , & ! Dcoeffs(gC4H7,gPXC7H15)
       0.000100115970995278_WP , & ! Dcoeffs(gPXC4H9,gPXC7H15)
       8.92946573258037e-05_WP , & ! Dcoeffs(gPXC5H11,gPXC7H15)
       7.41894287645355e-05_WP , & ! Dcoeffs(gPXC7H15,gPXC7H15)
       5.59251182229785e-05_WP , & ! Dcoeffs(gPXC12H25,gPXC7H15)
       5.59251182229785e-05_WP , & ! Dcoeffs(gS3XC12H25,gPXC7H15)
       5.59251182229785e-05_WP , & ! Dcoeffs(gSXC12H25,gPXC7H15)
       5.3803938046675e-05_WP , & ! Dcoeffs(gC12OOH,gPXC7H15)
       4.98356481410343e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gPXC7H15)
       0.00013500681303601_WP , & ! Dcoeffs(gN2,gPXC12H25)
       0.000205287800385824_WP , & ! Dcoeffs(gO,gPXC12H25)
       0.000537277105115026_WP , & ! Dcoeffs(gH2,gPXC12H25)
       0.000909420146641455_WP , & ! Dcoeffs(gH,gPXC12H25)
       0.000199652349138125_WP , & ! Dcoeffs(gOH,gPXC12H25)
       0.000200398751464357_WP , & ! Dcoeffs(gH2O,gPXC12H25)
       0.000128268969620989_WP , & ! Dcoeffs(gH2O2,gPXC12H25)
       0.000131590507316762_WP , & ! Dcoeffs(gO2,gPXC12H25)
       0.000129889629651748_WP , & ! Dcoeffs(gHO2,gPXC12H25)
       0.000131845377268614_WP , & ! Dcoeffs(gCH2O,gPXC12H25)
       0.000109080494359155_WP , & ! Dcoeffs(gCO2,gPXC12H25)
       0.000172312955827666_WP , & ! Dcoeffs(gCH3,gPXC12H25)
       0.000134296351757198_WP , & ! Dcoeffs(gCO,gPXC12H25)
       0.000115752465131257_WP , & ! Dcoeffs(gC2H6,gPXC12H25)
       0.000168944845599797_WP , & ! Dcoeffs(gCH4,gPXC12H25)
       0.000126503785867108_WP , & ! Dcoeffs(gC2H4,gPXC12H25)
       0.000127631668438687_WP , & ! Dcoeffs(gC2H2,gPXC12H25)
       8.96833961226298e-05_WP , & ! Dcoeffs(gC3H6,gPXC12H25)
       7.76775899332576e-05_WP , & ! Dcoeffs(gC4H81,gPXC12H25)
       6.96997464711787e-05_WP , & ! Dcoeffs(gC5H10,gPXC12H25)
       6.25429190487523e-05_WP , & ! Dcoeffs(gC6H12,gPXC12H25)
       5.72227985645459e-05_WP , & ! Dcoeffs(gC7H14,gPXC12H25)
       5.30571311938653e-05_WP , & ! Dcoeffs(gC8H16,gPXC12H25)
       4.94989593436907e-05_WP , & ! Dcoeffs(gC9H18,gPXC12H25)
       4.6333457758663e-05_WP , & ! Dcoeffs(gC10H20,gPXC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12H25O2,gPXC12H25)
       4.1157707466652e-05_WP , & ! Dcoeffs(gNXC12H26,gPXC12H25)
       3.76971391844953e-05_WP , & ! Dcoeffs(gOC12H23OOH,gPXC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2,gPXC12H25)
       0.000133776278788218_WP , & ! Dcoeffs(gHCO,gPXC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2D,gPXC12H25)
       0.000127603453260587_WP , & ! Dcoeffs(gCH3O,gPXC12H25)
       0.000125553167077626_WP , & ! Dcoeffs(gC2H3,gPXC12H25)
       0.000105951141788741_WP , & ! Dcoeffs(gCH2CHO,gPXC12H25)
       0.000117444896359053_WP , & ! Dcoeffs(gC2H5,gPXC12H25)
       9.05606119724398e-05_WP , & ! Dcoeffs(gAXC3H5,gPXC12H25)
       7.76994420481365e-05_WP , & ! Dcoeffs(gC2H3CHO,gPXC12H25)
       8.88391623340632e-05_WP , & ! Dcoeffs(gNXC3H7,gPXC12H25)
       7.82094898636555e-05_WP , & ! Dcoeffs(gC4H7,gPXC12H25)
       7.71609909181268e-05_WP , & ! Dcoeffs(gPXC4H9,gPXC12H25)
       6.82107840618616e-05_WP , & ! Dcoeffs(gPXC5H11,gPXC12H25)
       5.59251182229785e-05_WP , & ! Dcoeffs(gPXC7H15,gPXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gPXC12H25,gPXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gS3XC12H25,gPXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gSXC12H25,gPXC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12OOH,gPXC12H25)
       3.61538952732739e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gPXC12H25)
       0.00013500681303601_WP , & ! Dcoeffs(gN2,gS3XC12H25)
       0.000205287800385824_WP , & ! Dcoeffs(gO,gS3XC12H25)
       0.000537277105115026_WP , & ! Dcoeffs(gH2,gS3XC12H25)
       0.000909420146641455_WP , & ! Dcoeffs(gH,gS3XC12H25)
       0.000199652349138125_WP , & ! Dcoeffs(gOH,gS3XC12H25)
       0.000200398751464357_WP , & ! Dcoeffs(gH2O,gS3XC12H25)
       0.000128268969620989_WP , & ! Dcoeffs(gH2O2,gS3XC12H25)
       0.000131590507316762_WP , & ! Dcoeffs(gO2,gS3XC12H25)
       0.000129889629651748_WP , & ! Dcoeffs(gHO2,gS3XC12H25)
       0.000131845377268614_WP , & ! Dcoeffs(gCH2O,gS3XC12H25)
       0.000109080494359155_WP , & ! Dcoeffs(gCO2,gS3XC12H25)
       0.000172312955827666_WP , & ! Dcoeffs(gCH3,gS3XC12H25)
       0.000134296351757198_WP , & ! Dcoeffs(gCO,gS3XC12H25)
       0.000115752465131257_WP , & ! Dcoeffs(gC2H6,gS3XC12H25)
       0.000168944845599797_WP , & ! Dcoeffs(gCH4,gS3XC12H25)
       0.000126503785867108_WP , & ! Dcoeffs(gC2H4,gS3XC12H25)
       0.000127631668438687_WP , & ! Dcoeffs(gC2H2,gS3XC12H25)
       8.96833961226298e-05_WP , & ! Dcoeffs(gC3H6,gS3XC12H25)
       7.76775899332576e-05_WP , & ! Dcoeffs(gC4H81,gS3XC12H25)
       6.96997464711787e-05_WP , & ! Dcoeffs(gC5H10,gS3XC12H25)
       6.25429190487523e-05_WP , & ! Dcoeffs(gC6H12,gS3XC12H25)
       5.72227985645459e-05_WP , & ! Dcoeffs(gC7H14,gS3XC12H25)
       5.30571311938653e-05_WP , & ! Dcoeffs(gC8H16,gS3XC12H25)
       4.94989593436907e-05_WP , & ! Dcoeffs(gC9H18,gS3XC12H25)
       4.6333457758663e-05_WP , & ! Dcoeffs(gC10H20,gS3XC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12H25O2,gS3XC12H25)
       4.1157707466652e-05_WP , & ! Dcoeffs(gNXC12H26,gS3XC12H25)
       3.76971391844953e-05_WP , & ! Dcoeffs(gOC12H23OOH,gS3XC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2,gS3XC12H25)
       0.000133776278788218_WP , & ! Dcoeffs(gHCO,gS3XC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2D,gS3XC12H25)
       0.000127603453260587_WP , & ! Dcoeffs(gCH3O,gS3XC12H25)
       0.000125553167077626_WP , & ! Dcoeffs(gC2H3,gS3XC12H25)
       0.000105951141788741_WP , & ! Dcoeffs(gCH2CHO,gS3XC12H25)
       0.000117444896359053_WP , & ! Dcoeffs(gC2H5,gS3XC12H25)
       9.05606119724398e-05_WP , & ! Dcoeffs(gAXC3H5,gS3XC12H25)
       7.76994420481365e-05_WP , & ! Dcoeffs(gC2H3CHO,gS3XC12H25)
       8.88391623340632e-05_WP , & ! Dcoeffs(gNXC3H7,gS3XC12H25)
       7.82094898636555e-05_WP , & ! Dcoeffs(gC4H7,gS3XC12H25)
       7.71609909181268e-05_WP , & ! Dcoeffs(gPXC4H9,gS3XC12H25)
       6.82107840618616e-05_WP , & ! Dcoeffs(gPXC5H11,gS3XC12H25)
       5.59251182229785e-05_WP , & ! Dcoeffs(gPXC7H15,gS3XC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gPXC12H25,gS3XC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gS3XC12H25,gS3XC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gSXC12H25,gS3XC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12OOH,gS3XC12H25)
       3.61538952732739e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gS3XC12H25)
       0.00013500681303601_WP , & ! Dcoeffs(gN2,gSXC12H25)
       0.000205287800385824_WP , & ! Dcoeffs(gO,gSXC12H25)
       0.000537277105115026_WP , & ! Dcoeffs(gH2,gSXC12H25)
       0.000909420146641455_WP , & ! Dcoeffs(gH,gSXC12H25)
       0.000199652349138125_WP , & ! Dcoeffs(gOH,gSXC12H25)
       0.000200398751464357_WP , & ! Dcoeffs(gH2O,gSXC12H25)
       0.000128268969620989_WP , & ! Dcoeffs(gH2O2,gSXC12H25)
       0.000131590507316762_WP , & ! Dcoeffs(gO2,gSXC12H25)
       0.000129889629651748_WP , & ! Dcoeffs(gHO2,gSXC12H25)
       0.000131845377268614_WP , & ! Dcoeffs(gCH2O,gSXC12H25)
       0.000109080494359155_WP , & ! Dcoeffs(gCO2,gSXC12H25)
       0.000172312955827666_WP , & ! Dcoeffs(gCH3,gSXC12H25)
       0.000134296351757198_WP , & ! Dcoeffs(gCO,gSXC12H25)
       0.000115752465131257_WP , & ! Dcoeffs(gC2H6,gSXC12H25)
       0.000168944845599797_WP , & ! Dcoeffs(gCH4,gSXC12H25)
       0.000126503785867108_WP , & ! Dcoeffs(gC2H4,gSXC12H25)
       0.000127631668438687_WP , & ! Dcoeffs(gC2H2,gSXC12H25)
       8.96833961226298e-05_WP , & ! Dcoeffs(gC3H6,gSXC12H25)
       7.76775899332576e-05_WP , & ! Dcoeffs(gC4H81,gSXC12H25)
       6.96997464711787e-05_WP , & ! Dcoeffs(gC5H10,gSXC12H25)
       6.25429190487523e-05_WP , & ! Dcoeffs(gC6H12,gSXC12H25)
       5.72227985645459e-05_WP , & ! Dcoeffs(gC7H14,gSXC12H25)
       5.30571311938653e-05_WP , & ! Dcoeffs(gC8H16,gSXC12H25)
       4.94989593436907e-05_WP , & ! Dcoeffs(gC9H18,gSXC12H25)
       4.6333457758663e-05_WP , & ! Dcoeffs(gC10H20,gSXC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12H25O2,gSXC12H25)
       4.1157707466652e-05_WP , & ! Dcoeffs(gNXC12H26,gSXC12H25)
       3.76971391844953e-05_WP , & ! Dcoeffs(gOC12H23OOH,gSXC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2,gSXC12H25)
       0.000133776278788218_WP , & ! Dcoeffs(gHCO,gSXC12H25)
       0.000177908920783083_WP , & ! Dcoeffs(gCH2D,gSXC12H25)
       0.000127603453260587_WP , & ! Dcoeffs(gCH3O,gSXC12H25)
       0.000125553167077626_WP , & ! Dcoeffs(gC2H3,gSXC12H25)
       0.000105951141788741_WP , & ! Dcoeffs(gCH2CHO,gSXC12H25)
       0.000117444896359053_WP , & ! Dcoeffs(gC2H5,gSXC12H25)
       9.05606119724398e-05_WP , & ! Dcoeffs(gAXC3H5,gSXC12H25)
       7.76994420481365e-05_WP , & ! Dcoeffs(gC2H3CHO,gSXC12H25)
       8.88391623340632e-05_WP , & ! Dcoeffs(gNXC3H7,gSXC12H25)
       7.82094898636555e-05_WP , & ! Dcoeffs(gC4H7,gSXC12H25)
       7.71609909181268e-05_WP , & ! Dcoeffs(gPXC4H9,gSXC12H25)
       6.82107840618616e-05_WP , & ! Dcoeffs(gPXC5H11,gSXC12H25)
       5.59251182229785e-05_WP , & ! Dcoeffs(gPXC7H15,gSXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gPXC12H25,gSXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gS3XC12H25,gSXC12H25)
       4.12187356983576e-05_WP , & ! Dcoeffs(gSXC12H25,gSXC12H25)
       3.92400355888598e-05_WP , & ! Dcoeffs(gC12OOH,gSXC12H25)
       3.61538952732739e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gSXC12H25)
       0.000132108906772413_WP , & ! Dcoeffs(gN2,gC12OOH)
       0.000201604361130571_WP , & ! Dcoeffs(gO,gC12OOH)
       0.000530899051369924_WP , & ! Dcoeffs(gH2,gC12OOH)
       0.000898099753942287_WP , & ! Dcoeffs(gH,gC12OOH)
       0.000195991916812534_WP , & ! Dcoeffs(gOH,gC12OOH)
       0.000196614066548498_WP , & ! Dcoeffs(gH2O,gC12OOH)
       0.000125237271721254_WP , & ! Dcoeffs(gH2O2,gC12OOH)
       0.000128567746090024_WP , & ! Dcoeffs(gO2,gC12OOH)
       0.000126862575292341_WP , & ! Dcoeffs(gHO2,gC12OOH)
       0.000128920870476707_WP , & ! Dcoeffs(gCH2O,gC12OOH)
       0.000106193774124665_WP , & ! Dcoeffs(gCO2,gC12OOH)
       0.000169469562465148_WP , & ! Dcoeffs(gCH3,gC12OOH)
       0.000131417814874274_WP , & ! Dcoeffs(gCO,gC12OOH)
       0.000113256360163701_WP , & ! Dcoeffs(gC2H6,gC12OOH)
       0.000166081811727536_WP , & ! Dcoeffs(gCH4,gC12OOH)
       0.000123827344024322_WP , & ! Dcoeffs(gC2H4,gC12OOH)
       0.000125035637840811_WP , & ! Dcoeffs(gC2H2,gC12OOH)
       8.74517283125637e-05_WP , & ! Dcoeffs(gC3H6,gC12OOH)
       7.54451312148859e-05_WP , & ! Dcoeffs(gC4H81,gC12OOH)
       6.74577529817375e-05_WP , & ! Dcoeffs(gC5H10,gC12OOH)
       6.03454213138423e-05_WP , & ! Dcoeffs(gC6H12,gC12OOH)
       5.50588655178693e-05_WP , & ! Dcoeffs(gC7H14,gC12OOH)
       5.09220255548948e-05_WP , & ! Dcoeffs(gC8H16,gC12OOH)
       4.73986665738814e-05_WP , & ! Dcoeffs(gC9H18,gC12OOH)
       4.42757551534723e-05_WP , & ! Dcoeffs(gC10H20,gC12OOH)
       3.72179581018383e-05_WP , & ! Dcoeffs(gC12H25O2,gC12OOH)
       3.91769169035378e-05_WP , & ! Dcoeffs(gNXC12H26,gC12OOH)
       3.57025916723498e-05_WP , & ! Dcoeffs(gOC12H23OOH,gC12OOH)
       0.000175044312379159_WP , & ! Dcoeffs(gCH2,gC12OOH)
       0.000130854911935186_WP , & ! Dcoeffs(gHCO,gC12OOH)
       0.000175044312379159_WP , & ! Dcoeffs(gCH2D,gC12OOH)
       0.000124741567618169_WP , & ! Dcoeffs(gCH3O,gC12OOH)
       0.000122954971223631_WP , & ! Dcoeffs(gC2H3,gC12OOH)
       0.000103197462637561_WP , & ! Dcoeffs(gCH2CHO,gC12OOH)
       0.000114952663045108_WP , & ! Dcoeffs(gC2H5,gC12OOH)
       8.83349234013819e-05_WP , & ! Dcoeffs(gAXC3H5,gC12OOH)
       7.5467229380022e-05_WP , & ! Dcoeffs(gC2H3CHO,gC12OOH)
       8.66014735991708e-05_WP , & ! Dcoeffs(gNXC3H7,gC12OOH)
       7.59829480247145e-05_WP , & ! Dcoeffs(gC4H7,gC12OOH)
       7.49226403238765e-05_WP , & ! Dcoeffs(gPXC4H9,gC12OOH)
       6.60051819681908e-05_WP , & ! Dcoeffs(gPXC5H11,gC12OOH)
       5.3803938046675e-05_WP , & ! Dcoeffs(gPXC7H15,gC12OOH)
       3.92400355888598e-05_WP , & ! Dcoeffs(gPXC12H25,gC12OOH)
       3.92400355888598e-05_WP , & ! Dcoeffs(gS3XC12H25,gC12OOH)
       3.92400355888598e-05_WP , & ! Dcoeffs(gSXC12H25,gC12OOH)
       3.72179581018383e-05_WP , & ! Dcoeffs(gC12OOH,gC12OOH)
       3.41888617550563e-05_WP , & ! Dcoeffs(gO2C12H24OOH,gC12OOH)
       0.000122736152591483_WP , & ! Dcoeffs(gN2,gO2C12H24OOH)
       0.00018687391635693_WP , & ! Dcoeffs(gO,gO2C12H24OOH)
       0.000494859184882681_WP , & ! Dcoeffs(gH2,gO2C12H24OOH)
       0.000831995903743664_WP , & ! Dcoeffs(gH,gO2C12H24OOH)
       0.000181617716402296_WP , & ! Dcoeffs(gOH,gO2C12H24OOH)
       0.000181951602962548_WP , & ! Dcoeffs(gH2O,gO2C12H24OOH)
       0.000116055565039968_WP , & ! Dcoeffs(gH2O2,gO2C12H24OOH)
       0.000119203463484425_WP , & ! Dcoeffs(gO2,gO2C12H24OOH)
       0.000117591973093396_WP , & ! Dcoeffs(gHO2,gO2C12H24OOH)
       0.000119688455847605_WP , & ! Dcoeffs(gCH2O,gO2C12H24OOH)
       9.83475118109865e-05_WP , & ! Dcoeffs(gCO2,gO2C12H24OOH)
       0.000158190108342894_WP , & ! Dcoeffs(gCH3,gO2C12H24OOH)
       0.000122115526183795_WP , & ! Dcoeffs(gCO,gO2C12H24OOH)
       0.000105567755934202_WP , & ! Dcoeffs(gC2H6,gO2C12H24OOH)
       0.000154932866960863_WP , & ! Dcoeffs(gCH4,gO2C12H24OOH)
       0.000115274567391594_WP , & ! Dcoeffs(gC2H4,gO2C12H24OOH)
       0.000116545937800143_WP , & ! Dcoeffs(gC2H2,gO2C12H24OOH)
       8.15470509198558e-05_WP , & ! Dcoeffs(gC3H6,gO2C12H24OOH)
       7.01913692648357e-05_WP , & ! Dcoeffs(gC4H81,gO2C12H24OOH)
       6.26276221057583e-05_WP , & ! Dcoeffs(gC5H10,gO2C12H24OOH)
       5.59489322285093e-05_WP , & ! Dcoeffs(gC6H12,gO2C12H24OOH)
       5.09797533571647e-05_WP , & ! Dcoeffs(gC7H14,gO2C12H24OOH)
       4.70900193992173e-05_WP , & ! Dcoeffs(gC8H16,gO2C12H24OOH)
       4.37842273156116e-05_WP , & ! Dcoeffs(gC9H18,gO2C12H24OOH)
       4.08626386056184e-05_WP , & ! Dcoeffs(gC10H20,gO2C12H24OOH)
       3.41888617550563e-05_WP , & ! Dcoeffs(gC12H25O2,gO2C12H24OOH)
       3.60918501787869e-05_WP , & ! Dcoeffs(gNXC12H26,gO2C12H24OOH)
       3.27724685244368e-05_WP , & ! Dcoeffs(gOC12H23OOH,gO2C12H24OOH)
       0.00016344308285273_WP , & ! Dcoeffs(gCH2,gO2C12H24OOH)
       0.000121516290789697_WP , & ! Dcoeffs(gHCO,gO2C12H24OOH)
       0.00016344308285273_WP , & ! Dcoeffs(gCH2D,gO2C12H24OOH)
       0.000115846898719432_WP , & ! Dcoeffs(gCH3O,gO2C12H24OOH)
       0.000114575332534957_WP , & ! Dcoeffs(gC2H3,gO2C12H24OOH)
       9.57075762085096e-05_WP , & ! Dcoeffs(gCH2CHO,gO2C12H24OOH)
       0.000107177377752011_WP , & ! Dcoeffs(gC2H5,gO2C12H24OOH)
       8.23905120236073e-05_WP , & ! Dcoeffs(gAXC3H5,gO2C12H24OOH)
       7.02125619648556e-05_WP , & ! Dcoeffs(gC2H3CHO,gO2C12H24OOH)
       8.07348528249231e-05_WP , & ! Dcoeffs(gNXC3H7,gO2C12H24OOH)
       7.07070953010208e-05_WP , & ! Dcoeffs(gC4H7,gO2C12H24OOH)
       6.96902311602927e-05_WP , & ! Dcoeffs(gPXC4H9,gO2C12H24OOH)
       6.12950132778709e-05_WP , & ! Dcoeffs(gPXC5H11,gO2C12H24OOH)
       4.98356481410343e-05_WP , & ! Dcoeffs(gPXC7H15,gO2C12H24OOH)
       3.61538952732739e-05_WP , & ! Dcoeffs(gPXC12H25,gO2C12H24OOH)
       3.61538952732739e-05_WP , & ! Dcoeffs(gS3XC12H25,gO2C12H24OOH)
       3.61538952732739e-05_WP , & ! Dcoeffs(gSXC12H25,gO2C12H24OOH)
       3.41888617550563e-05_WP , & ! Dcoeffs(gC12OOH,gO2C12H24OOH)
       3.13583403446466e-05_WP  & ! Dcoeffs(gO2C12H24OOH,gO2C12H24OOH)
  /), (/npS,npS/) )

  ! Omega_coeffs (k over epsilon^2)s 
  real(WP), parameter, dimension(npS,npS) :: Ocoeffs = reshape ( (/ &
       0.0102532554085922_WP , & ! Ocoeffs(gN2,gN2)
       0.0113210287786668_WP , & ! Ocoeffs(gO,gN2)
       0.0164262750114695_WP , & ! Ocoeffs(gH2,gN2)
       0.0084090490702667_WP , & ! Ocoeffs(gH,gN2)
       0.0113210287786668_WP , & ! Ocoeffs(gOH,gN2)
       0.00423234512983402_WP , & ! Ocoeffs(gH2O,gN2)
       0.00977076903085442_WP , & ! Ocoeffs(gH2O2,gN2)
       0.00977076903085442_WP , & ! Ocoeffs(gO2,gN2)
       0.00977076903085442_WP , & ! Ocoeffs(gHO2,gN2)
       0.0045374955958451_WP , & ! Ocoeffs(gCH2O,gN2)
       0.006482402221384_WP , & ! Ocoeffs(gCO2,gN2)
       0.00843819664144349_WP , & ! Ocoeffs(gCH3,gN2)
       0.0102234242676958_WP , & ! Ocoeffs(gCO,gN2)
       0.00637488364852818_WP , & ! Ocoeffs(gC2H6,gN2)
       0.00851542215559055_WP , & ! Ocoeffs(gCH4,gN2)
       0.00604271812500727_WP , & ! Ocoeffs(gC2H4,gN2)
       0.00700418719899912_WP , & ! Ocoeffs(gC2H2,gN2)
       0.00619923322161321_WP , & ! Ocoeffs(gC3H6,gN2)
       0.00535916069785301_WP , & ! Ocoeffs(gC4H81,gN2)
       0.00478129776306122_WP , & ! Ocoeffs(gC5H10,gN2)
       0.00450759393044929_WP , & ! Ocoeffs(gC6H12,gN2)
       0.00428681177044489_WP , & ! Ocoeffs(gC7H14,gN2)
       0.00410492171339442_WP , & ! Ocoeffs(gC8H16,gN2)
       0.00395531414675628_WP , & ! Ocoeffs(gC9H18,gN2)
       0.0038323505332362_WP , & ! Ocoeffs(gC10H20,gN2)
       0.00356881952018071_WP , & ! Ocoeffs(gC12H25O2,gN2)
       0.00360265636722638_WP , & ! Ocoeffs(gNXC12H26,gN2)
       0.00349510202689907_WP , & ! Ocoeffs(gOC12H23OOH,gN2)
       0.00843819664144349_WP , & ! Ocoeffs(gCH2,gN2)
       0.0045374955958451_WP , & ! Ocoeffs(gHCO,gN2)
       0.00843819664144349_WP , & ! Ocoeffs(gCH2D,gN2)
       0.00495864320824235_WP , & ! Ocoeffs(gCH3O,gN2)
       0.00700418719899912_WP , & ! Ocoeffs(gC2H3,gN2)
       0.00484939592582365_WP , & ! Ocoeffs(gCH2CHO,gN2)
       0.00637488364852818_WP , & ! Ocoeffs(gC2H5,gN2)
       0.00619923322161321_WP , & ! Ocoeffs(gAXC3H5,gN2)
       0.00535916069785301_WP , & ! Ocoeffs(gC2H3CHO,gN2)
       0.00619923322161321_WP , & ! Ocoeffs(gNXC3H7,gN2)
       0.00535916069785301_WP , & ! Ocoeffs(gC4H7,gN2)
       0.00535916069785301_WP , & ! Ocoeffs(gPXC4H9,gN2)
       0.00473055260241948_WP , & ! Ocoeffs(gPXC5H11,gN2)
       0.004263632714178_WP , & ! Ocoeffs(gPXC7H15,gN2)
       0.00360265636722638_WP , & ! Ocoeffs(gPXC12H25,gN2)
       0.00360265636722638_WP , & ! Ocoeffs(gS3XC12H25,gN2)
       0.00360265636722638_WP , & ! Ocoeffs(gSXC12H25,gN2)
       0.00356881952018071_WP , & ! Ocoeffs(gC12OOH,gN2)
       0.00341915049060909_WP , & ! Ocoeffs(gO2C12H24OOH,gN2)
       0.0113210287786668_WP , & ! Ocoeffs(gN2,gO)
       0.0125_WP , & ! Ocoeffs(gO,gO)
       0.0181369062527503_WP , & ! Ocoeffs(gH2,gO)
       0.00928476690885259_WP , & ! Ocoeffs(gH,gO)
       0.0125_WP , & ! Ocoeffs(gOH,gO)
       0.00467310128410037_WP , & ! Ocoeffs(gH2O,gO)
       0.0107882963000526_WP , & ! Ocoeffs(gH2O2,gO)
       0.0107882963000526_WP , & ! Ocoeffs(gO2,gO)
       0.0107882963000526_WP , & ! Ocoeffs(gHO2,gO)
       0.00501003010035126_WP , & ! Ocoeffs(gCH2O,gO)
       0.00715747917892335_WP , & ! Ocoeffs(gCO2,gO)
       0.00931694990624912_WP , & ! Ocoeffs(gCH3,gO)
       0.0112880910246433_WP , & ! Ocoeffs(gCO,gO)
       0.00703876362868729_WP , & ! Ocoeffs(gC2H6,gO)
       0.00940221768055752_WP , & ! Ocoeffs(gCH4,gO)
       0.0066720064085453_WP , & ! Ocoeffs(gC2H4,gO)
       0.00773360281112183_WP , & ! Ocoeffs(gC2H2,gO)
       0.00684482097741742_WP , & ! Ocoeffs(gC3H6,gO)
       0.00591726335413939_WP , & ! Ocoeffs(gC4H81,gO)
       0.00527922180984892_WP , & ! Ocoeffs(gC5H10,gO)
       0.00497701447741141_WP , & ! Ocoeffs(gC6H12,gO)
       0.00473324007722126_WP , & ! Ocoeffs(gC7H14,gO)
       0.00453240800112803_WP , & ! Ocoeffs(gC8H16,gO)
       0.00436722031195788_WP , & ! Ocoeffs(gC9H18,gO)
       0.00423145127549916_WP , & ! Ocoeffs(gC10H20,gO)
       0.00394047615940362_WP , & ! Ocoeffs(gC12H25O2,gO)
       0.00397783677356156_WP , & ! Ocoeffs(gNXC12H26,gO)
       0.0038590817310318_WP , & ! Ocoeffs(gOC12H23OOH,gO)
       0.00931694990624912_WP , & ! Ocoeffs(gCH2,gO)
       0.00501003010035126_WP , & ! Ocoeffs(gHCO,gO)
       0.00931694990624912_WP , & ! Ocoeffs(gCH2D,gO)
       0.00547503599848004_WP , & ! Ocoeffs(gCH3O,gO)
       0.00773360281112183_WP , & ! Ocoeffs(gC2H3,gO)
       0.00535441171097649_WP , & ! Ocoeffs(gCH2CHO,gO)
       0.00703876362868729_WP , & ! Ocoeffs(gC2H5,gO)
       0.00684482097741742_WP , & ! Ocoeffs(gAXC3H5,gO)
       0.00591726335413939_WP , & ! Ocoeffs(gC2H3CHO,gO)
       0.00684482097741742_WP , & ! Ocoeffs(gNXC3H7,gO)
       0.00591726335413939_WP , & ! Ocoeffs(gC4H7,gO)
       0.00591726335413939_WP , & ! Ocoeffs(gPXC4H9,gO)
       0.00522319205138588_WP , & ! Ocoeffs(gPXC5H11,gO)
       0.00470764715550003_WP , & ! Ocoeffs(gPXC7H15,gO)
       0.00397783677356156_WP , & ! Ocoeffs(gPXC12H25,gO)
       0.00397783677356156_WP , & ! Ocoeffs(gS3XC12H25,gO)
       0.00397783677356156_WP , & ! Ocoeffs(gSXC12H25,gO)
       0.00394047615940362_WP , & ! Ocoeffs(gC12OOH,gO)
       0.00377522060655397_WP , & ! Ocoeffs(gO2C12H24OOH,gO)
       0.0164262750114695_WP , & ! Ocoeffs(gN2,gH2)
       0.0181369062527503_WP , & ! Ocoeffs(gO,gH2)
       0.0263157894736842_WP , & ! Ocoeffs(gH2,gH2)
       0.0134717557603598_WP , & ! Ocoeffs(gH,gH2)
       0.0181369062527503_WP , & ! Ocoeffs(gOH,gH2)
       0.00678044799194683_WP , & ! Ocoeffs(gH2O,gH2)
       0.0156533054896757_WP , & ! Ocoeffs(gH2O2,gH2)
       0.0156533054896757_WP , & ! Ocoeffs(gO2,gH2)
       0.0156533054896757_WP , & ! Ocoeffs(gHO2,gH2)
       0.00726931570028224_WP , & ! Ocoeffs(gCH2O,gH2)
       0.0103851623099316_WP , & ! Ocoeffs(gCO2,gH2)
       0.0135184517608969_WP , & ! Ocoeffs(gCH3,gH2)
       0.0163784838949174_WP , & ! Ocoeffs(gCO,gH2)
       0.0102129116855016_WP , & ! Ocoeffs(gC2H6,gH2)
       0.0136421712512178_WP , & ! Ocoeffs(gCH4,gH2)
       0.00968076437996282_WP , & ! Ocoeffs(gC2H4,gH2)
       0.0112210903345058_WP , & ! Ocoeffs(gC2H2,gH2)
       0.00993151011074227_WP , & ! Ocoeffs(gC3H6,gH2)
       0.00858566805814886_WP , & ! Ocoeffs(gC4H81,gH2)
       0.00765990008421637_WP , & ! Ocoeffs(gC5H10,gH2)
       0.00722141159963134_WP , & ! Ocoeffs(gC6H12,gH2)
       0.0068677065241858_WP , & ! Ocoeffs(gC7H14,gH2)
       0.00657630872125395_WP , & ! Ocoeffs(gC8H16,gH2)
       0.00633662923064696_WP , & ! Ocoeffs(gC9H18,gH2)
       0.00613963480774471_WP , & ! Ocoeffs(gC10H20,gH2)
       0.00571744373554407_WP , & ! Ocoeffs(gC12H25O2,gH2)
       0.0057716522120663_WP , & ! Ocoeffs(gNXC12H26,gH2)
       0.005599344286194_WP , & ! Ocoeffs(gOC12H23OOH,gH2)
       0.0135184517608969_WP , & ! Ocoeffs(gCH2,gH2)
       0.00726931570028224_WP , & ! Ocoeffs(gHCO,gH2)
       0.0135184517608969_WP , & ! Ocoeffs(gCH2D,gH2)
       0.00794401717078924_WP , & ! Ocoeffs(gCH3O,gH2)
       0.0112210903345058_WP , & ! Ocoeffs(gC2H3,gH2)
       0.00776899705924871_WP , & ! Ocoeffs(gCH2CHO,gH2)
       0.0102129116855016_WP , & ! Ocoeffs(gC2H5,gH2)
       0.00993151011074227_WP , & ! Ocoeffs(gAXC3H5,gH2)
       0.00858566805814886_WP , & ! Ocoeffs(gC2H3CHO,gH2)
       0.00993151011074227_WP , & ! Ocoeffs(gNXC3H7,gH2)
       0.00858566805814886_WP , & ! Ocoeffs(gC4H7,gH2)
       0.00858566805814886_WP , & ! Ocoeffs(gPXC4H9,gH2)
       0.00757860356608769_WP , & ! Ocoeffs(gPXC5H11,gH2)
       0.00683057241042645_WP , & ! Ocoeffs(gPXC7H15,gH2)
       0.0057716522120663_WP , & ! Ocoeffs(gPXC12H25,gH2)
       0.0057716522120663_WP , & ! Ocoeffs(gS3XC12H25,gH2)
       0.0057716522120663_WP , & ! Ocoeffs(gSXC12H25,gH2)
       0.00571744373554407_WP , & ! Ocoeffs(gC12OOH,gH2)
       0.00547766577796164_WP , & ! Ocoeffs(gO2C12H24OOH,gH2)
       0.0084090490702667_WP , & ! Ocoeffs(gN2,gH)
       0.00928476690885259_WP , & ! Ocoeffs(gO,gH)
       0.0134717557603598_WP , & ! Ocoeffs(gH2,gH)
       0.00689655172413793_WP , & ! Ocoeffs(gH,gH)
       0.00928476690885259_WP , & ! Ocoeffs(gOH,gH)
       0.00347109249314653_WP , & ! Ocoeffs(gH2O,gH)
       0.00801334531917_WP , & ! Ocoeffs(gH2O2,gH)
       0.00801334531917_WP , & ! Ocoeffs(gO2,gH)
       0.00801334531917_WP , & ! Ocoeffs(gHO2,gH)
       0.00372135693504775_WP , & ! Ocoeffs(gCH2O,gH)
       0.00531644206650152_WP , & ! Ocoeffs(gCO2,gH)
       0.00692045665447833_WP , & ! Ocoeffs(gCH3,gH)
       0.0083845835207779_WP , & ! Ocoeffs(gCO,gH)
       0.00522826236950968_WP , & ! Ocoeffs(gC2H6,gH)
       0.00698379196722154_WP , & ! Ocoeffs(gCH4,gH)
       0.0049558419454171_WP , & ! Ocoeffs(gC2H4,gH)
       0.00574437595735306_WP , & ! Ocoeffs(gC2H2,gH)
       0.00508420538465163_WP , & ! Ocoeffs(gC3H6,gH)
       0.00439523287851836_WP , & ! Ocoeffs(gC4H81,gH)
       0.00392130751716625_WP , & ! Ocoeffs(gC5H10,gH)
       0.00369683354597998_WP , & ! Ocoeffs(gC6H12,gH)
       0.0035157624672511_WP , & ! Ocoeffs(gC7H14,gH)
       0.00336658814610338_WP , & ! Ocoeffs(gC8H16,gH)
       0.00324388981089084_WP , & ! Ocoeffs(gC9H18,gH)
       0.00314304310233413_WP , & ! Ocoeffs(gC10H20,gH)
       0.00292691221199626_WP , & ! Ocoeffs(gC12H25O2,gH)
       0.00295466297951851_WP , & ! Ocoeffs(gNXC12H26,gH)
       0.00286645394838733_WP , & ! Ocoeffs(gOC12H23OOH,gH)
       0.00692045665447833_WP , & ! Ocoeffs(gCH2,gH)
       0.00372135693504775_WP , & ! Ocoeffs(gHCO,gH)
       0.00692045665447833_WP , & ! Ocoeffs(gCH2D,gH)
       0.00406675464507713_WP , & ! Ocoeffs(gCH3O,gH)
       0.00574437595735306_WP , & ! Ocoeffs(gC2H3,gH)
       0.00397715717363579_WP , & ! Ocoeffs(gCH2CHO,gH)
       0.00522826236950968_WP , & ! Ocoeffs(gC2H5,gH)
       0.00508420538465163_WP , & ! Ocoeffs(gAXC3H5,gH)
       0.00439523287851836_WP , & ! Ocoeffs(gC2H3CHO,gH)
       0.00508420538465163_WP , & ! Ocoeffs(gNXC3H7,gH)
       0.00439523287851836_WP , & ! Ocoeffs(gC4H7,gH)
       0.00439523287851836_WP , & ! Ocoeffs(gPXC4H9,gH)
       0.00387968965738316_WP , & ! Ocoeffs(gPXC5H11,gH)
       0.00349675252223526_WP , & ! Ocoeffs(gPXC7H15,gH)
       0.00295466297951851_WP , & ! Ocoeffs(gPXC12H25,gH)
       0.00295466297951851_WP , & ! Ocoeffs(gS3XC12H25,gH)
       0.00295466297951851_WP , & ! Ocoeffs(gSXC12H25,gH)
       0.00292691221199626_WP , & ! Ocoeffs(gC12OOH,gH)
       0.00280416346890806_WP , & ! Ocoeffs(gO2C12H24OOH,gH)
       0.0113210287786668_WP , & ! Ocoeffs(gN2,gOH)
       0.0125_WP , & ! Ocoeffs(gO,gOH)
       0.0181369062527503_WP , & ! Ocoeffs(gH2,gOH)
       0.00928476690885259_WP , & ! Ocoeffs(gH,gOH)
       0.0125_WP , & ! Ocoeffs(gOH,gOH)
       0.00467310128410037_WP , & ! Ocoeffs(gH2O,gOH)
       0.0107882963000526_WP , & ! Ocoeffs(gH2O2,gOH)
       0.0107882963000526_WP , & ! Ocoeffs(gO2,gOH)
       0.0107882963000526_WP , & ! Ocoeffs(gHO2,gOH)
       0.00501003010035126_WP , & ! Ocoeffs(gCH2O,gOH)
       0.00715747917892335_WP , & ! Ocoeffs(gCO2,gOH)
       0.00931694990624912_WP , & ! Ocoeffs(gCH3,gOH)
       0.0112880910246433_WP , & ! Ocoeffs(gCO,gOH)
       0.00703876362868729_WP , & ! Ocoeffs(gC2H6,gOH)
       0.00940221768055752_WP , & ! Ocoeffs(gCH4,gOH)
       0.0066720064085453_WP , & ! Ocoeffs(gC2H4,gOH)
       0.00773360281112183_WP , & ! Ocoeffs(gC2H2,gOH)
       0.00684482097741742_WP , & ! Ocoeffs(gC3H6,gOH)
       0.00591726335413939_WP , & ! Ocoeffs(gC4H81,gOH)
       0.00527922180984892_WP , & ! Ocoeffs(gC5H10,gOH)
       0.00497701447741141_WP , & ! Ocoeffs(gC6H12,gOH)
       0.00473324007722126_WP , & ! Ocoeffs(gC7H14,gOH)
       0.00453240800112803_WP , & ! Ocoeffs(gC8H16,gOH)
       0.00436722031195788_WP , & ! Ocoeffs(gC9H18,gOH)
       0.00423145127549916_WP , & ! Ocoeffs(gC10H20,gOH)
       0.00394047615940362_WP , & ! Ocoeffs(gC12H25O2,gOH)
       0.00397783677356156_WP , & ! Ocoeffs(gNXC12H26,gOH)
       0.0038590817310318_WP , & ! Ocoeffs(gOC12H23OOH,gOH)
       0.00931694990624912_WP , & ! Ocoeffs(gCH2,gOH)
       0.00501003010035126_WP , & ! Ocoeffs(gHCO,gOH)
       0.00931694990624912_WP , & ! Ocoeffs(gCH2D,gOH)
       0.00547503599848004_WP , & ! Ocoeffs(gCH3O,gOH)
       0.00773360281112183_WP , & ! Ocoeffs(gC2H3,gOH)
       0.00535441171097649_WP , & ! Ocoeffs(gCH2CHO,gOH)
       0.00703876362868729_WP , & ! Ocoeffs(gC2H5,gOH)
       0.00684482097741742_WP , & ! Ocoeffs(gAXC3H5,gOH)
       0.00591726335413939_WP , & ! Ocoeffs(gC2H3CHO,gOH)
       0.00684482097741742_WP , & ! Ocoeffs(gNXC3H7,gOH)
       0.00591726335413939_WP , & ! Ocoeffs(gC4H7,gOH)
       0.00591726335413939_WP , & ! Ocoeffs(gPXC4H9,gOH)
       0.00522319205138588_WP , & ! Ocoeffs(gPXC5H11,gOH)
       0.00470764715550003_WP , & ! Ocoeffs(gPXC7H15,gOH)
       0.00397783677356156_WP , & ! Ocoeffs(gPXC12H25,gOH)
       0.00397783677356156_WP , & ! Ocoeffs(gS3XC12H25,gOH)
       0.00397783677356156_WP , & ! Ocoeffs(gSXC12H25,gOH)
       0.00394047615940362_WP , & ! Ocoeffs(gC12OOH,gOH)
       0.00377522060655397_WP , & ! Ocoeffs(gO2C12H24OOH,gOH)
       0.00423234512983402_WP , & ! Ocoeffs(gN2,gH2O)
       0.00467310128410037_WP , & ! Ocoeffs(gO,gH2O)
       0.00678044799194683_WP , & ! Ocoeffs(gH2,gH2O)
       0.00347109249314653_WP , & ! Ocoeffs(gH,gH2O)
       0.00467310128410037_WP , & ! Ocoeffs(gOH,gH2O)
       0.00174703004891684_WP , & ! Ocoeffs(gH2O,gH2O)
       0.00403318410344248_WP , & ! Ocoeffs(gH2O2,gH2O)
       0.00403318410344248_WP , & ! Ocoeffs(gO2,gH2O)
       0.00403318410344248_WP , & ! Ocoeffs(gHO2,gH2O)
       0.00187299024762664_WP , & ! Ocoeffs(gCH2O,gH2O)
       0.00267581001135587_WP , & ! Ocoeffs(gCO2,gH2O)
       0.00348312404566333_WP , & ! Ocoeffs(gCH3,gH2O)
       0.00422003141298419_WP , & ! Ocoeffs(gCO,gH2O)
       0.0026314284281358_WP , & ! Ocoeffs(gC2H6,gH2O)
       0.00351500124131236_WP , & ! Ocoeffs(gCH4,gH2O)
       0.00249431693722391_WP , & ! Ocoeffs(gC2H4,gH2O)
       0.00289119273819005_WP , & ! Ocoeffs(gC2H2,gH2O)
       0.00255892333592052_WP , & ! Ocoeffs(gC3H6,gH2O)
       0.00221215767828711_WP , & ! Ocoeffs(gC4H81,gH2O)
       0.00197362705749245_WP , & ! Ocoeffs(gC5H10,gH2O)
       0.00186064741963019_WP , & ! Ocoeffs(gC6H12,gH2O)
       0.00176951282262544_WP , & ! Ocoeffs(gC7H14,gH2O)
       0.00169443213201105_WP , & ! Ocoeffs(gC8H16,gH2O)
       0.00163267702782077_WP , & ! Ocoeffs(gC9H18,gH2O)
       0.00158192003113146_WP , & ! Ocoeffs(gC10H20,gH2O)
       0.00147313953603807_WP , & ! Ocoeffs(gC12H25O2,gH2O)
       0.00148710673075778_WP , & ! Ocoeffs(gNXC12H26,gH2O)
       0.00144271038341864_WP , & ! Ocoeffs(gOC12H23OOH,gH2O)
       0.00348312404566333_WP , & ! Ocoeffs(gCH2,gH2O)
       0.00187299024762664_WP , & ! Ocoeffs(gHCO,gH2O)
       0.00348312404566333_WP , & ! Ocoeffs(gCH2D,gH2O)
       0.00204683182039942_WP , & ! Ocoeffs(gCH3O,gH2O)
       0.00289119273819005_WP , & ! Ocoeffs(gC2H3,gH2O)
       0.0020017366593733_WP , & ! Ocoeffs(gCH2CHO,gH2O)
       0.0026314284281358_WP , & ! Ocoeffs(gC2H5,gH2O)
       0.00255892333592052_WP , & ! Ocoeffs(gAXC3H5,gH2O)
       0.00221215767828711_WP , & ! Ocoeffs(gC2H3CHO,gH2O)
       0.00255892333592052_WP , & ! Ocoeffs(gNXC3H7,gH2O)
       0.00221215767828711_WP , & ! Ocoeffs(gC4H7,gH2O)
       0.00221215767828711_WP , & ! Ocoeffs(gPXC4H9,gH2O)
       0.00195268043859474_WP , & ! Ocoeffs(gPXC5H11,gH2O)
       0.00175994495739669_WP , & ! Ocoeffs(gPXC7H15,gH2O)
       0.00148710673075778_WP , & ! Ocoeffs(gPXC12H25,gH2O)
       0.00148710673075778_WP , & ! Ocoeffs(gS3XC12H25,gH2O)
       0.00148710673075778_WP , & ! Ocoeffs(gSXC12H25,gH2O)
       0.00147313953603807_WP , & ! Ocoeffs(gC12OOH,gH2O)
       0.00141135906113996_WP , & ! Ocoeffs(gO2C12H24OOH,gH2O)
       0.00977076903085442_WP , & ! Ocoeffs(gN2,gH2O2)
       0.0107882963000526_WP , & ! Ocoeffs(gO,gH2O2)
       0.0156533054896757_WP , & ! Ocoeffs(gH2,gH2O2)
       0.00801334531917_WP , & ! Ocoeffs(gH,gH2O2)
       0.0107882963000526_WP , & ! Ocoeffs(gOH,gH2O2)
       0.00403318410344248_WP , & ! Ocoeffs(gH2O,gH2O2)
       0.00931098696461825_WP , & ! Ocoeffs(gH2O2,gH2O2)
       0.00931098696461825_WP , & ! Ocoeffs(gO2,gH2O2)
       0.00931098696461825_WP , & ! Ocoeffs(gHO2,gH2O2)
       0.00432397513558173_WP , & ! Ocoeffs(gCH2O,gH2O2)
       0.00617736049149457_WP , & ! Ocoeffs(gCO2,gH2O2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH3,gH2O2)
       0.00974234165086526_WP , & ! Ocoeffs(gCO,gH2O2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H6,gH2O2)
       0.00811471281723581_WP , & ! Ocoeffs(gCH4,gH2O2)
       0.0057583665640989_WP , & ! Ocoeffs(gC2H4,gH2O2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H2,gH2O2)
       0.00590751654601557_WP , & ! Ocoeffs(gC3H6,gH2O2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H81,gH2O2)
       0.004556304729468_WP , & ! Ocoeffs(gC5H10,gH2O2)
       0.00429548054975725_WP , & ! Ocoeffs(gC6H12,gH2O2)
       0.00408508771298773_WP , & ! Ocoeffs(gC7H14,gH2O2)
       0.00391175683751185_WP , & ! Ocoeffs(gC8H16,gH2O2)
       0.00376918933864077_WP , & ! Ocoeffs(gC9H18,gH2O2)
       0.00365201201114563_WP , & ! Ocoeffs(gC10H20,gH2O2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12H25O2,gH2O2)
       0.00343312653971418_WP , & ! Ocoeffs(gNXC12H26,gH2O2)
       0.00333063337283927_WP , & ! Ocoeffs(gOC12H23OOH,gH2O2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2,gH2O2)
       0.00432397513558173_WP , & ! Ocoeffs(gHCO,gH2O2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2D,gH2O2)
       0.00472530484840455_WP , & ! Ocoeffs(gCH3O,gH2O2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H3,gH2O2)
       0.00462119840403887_WP , & ! Ocoeffs(gCH2CHO,gH2O2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H5,gH2O2)
       0.00590751654601557_WP , & ! Ocoeffs(gAXC3H5,gH2O2)
       0.00510697522799189_WP , & ! Ocoeffs(gC2H3CHO,gH2O2)
       0.00590751654601557_WP , & ! Ocoeffs(gNXC3H7,gH2O2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H7,gH2O2)
       0.00510697522799189_WP , & ! Ocoeffs(gPXC4H9,gH2O2)
       0.00450794747859442_WP , & ! Ocoeffs(gPXC5H11,gH2O2)
       0.00406299939117072_WP , & ! Ocoeffs(gPXC7H15,gH2O2)
       0.00343312653971418_WP , & ! Ocoeffs(gPXC12H25,gH2O2)
       0.00343312653971418_WP , & ! Ocoeffs(gS3XC12H25,gH2O2)
       0.00343312653971418_WP , & ! Ocoeffs(gSXC12H25,gH2O2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12OOH,gH2O2)
       0.00325825588012548_WP , & ! Ocoeffs(gO2C12H24OOH,gH2O2)
       0.00977076903085442_WP , & ! Ocoeffs(gN2,gO2)
       0.0107882963000526_WP , & ! Ocoeffs(gO,gO2)
       0.0156533054896757_WP , & ! Ocoeffs(gH2,gO2)
       0.00801334531917_WP , & ! Ocoeffs(gH,gO2)
       0.0107882963000526_WP , & ! Ocoeffs(gOH,gO2)
       0.00403318410344248_WP , & ! Ocoeffs(gH2O,gO2)
       0.00931098696461825_WP , & ! Ocoeffs(gH2O2,gO2)
       0.00931098696461825_WP , & ! Ocoeffs(gO2,gO2)
       0.00931098696461825_WP , & ! Ocoeffs(gHO2,gO2)
       0.00432397513558173_WP , & ! Ocoeffs(gCH2O,gO2)
       0.00617736049149457_WP , & ! Ocoeffs(gCO2,gO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH3,gO2)
       0.00974234165086526_WP , & ! Ocoeffs(gCO,gO2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H6,gO2)
       0.00811471281723581_WP , & ! Ocoeffs(gCH4,gO2)
       0.0057583665640989_WP , & ! Ocoeffs(gC2H4,gO2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H2,gO2)
       0.00590751654601557_WP , & ! Ocoeffs(gC3H6,gO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H81,gO2)
       0.004556304729468_WP , & ! Ocoeffs(gC5H10,gO2)
       0.00429548054975725_WP , & ! Ocoeffs(gC6H12,gO2)
       0.00408508771298773_WP , & ! Ocoeffs(gC7H14,gO2)
       0.00391175683751185_WP , & ! Ocoeffs(gC8H16,gO2)
       0.00376918933864077_WP , & ! Ocoeffs(gC9H18,gO2)
       0.00365201201114563_WP , & ! Ocoeffs(gC10H20,gO2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12H25O2,gO2)
       0.00343312653971418_WP , & ! Ocoeffs(gNXC12H26,gO2)
       0.00333063337283927_WP , & ! Ocoeffs(gOC12H23OOH,gO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2,gO2)
       0.00432397513558173_WP , & ! Ocoeffs(gHCO,gO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2D,gO2)
       0.00472530484840455_WP , & ! Ocoeffs(gCH3O,gO2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H3,gO2)
       0.00462119840403887_WP , & ! Ocoeffs(gCH2CHO,gO2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H5,gO2)
       0.00590751654601557_WP , & ! Ocoeffs(gAXC3H5,gO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC2H3CHO,gO2)
       0.00590751654601557_WP , & ! Ocoeffs(gNXC3H7,gO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H7,gO2)
       0.00510697522799189_WP , & ! Ocoeffs(gPXC4H9,gO2)
       0.00450794747859442_WP , & ! Ocoeffs(gPXC5H11,gO2)
       0.00406299939117072_WP , & ! Ocoeffs(gPXC7H15,gO2)
       0.00343312653971418_WP , & ! Ocoeffs(gPXC12H25,gO2)
       0.00343312653971418_WP , & ! Ocoeffs(gS3XC12H25,gO2)
       0.00343312653971418_WP , & ! Ocoeffs(gSXC12H25,gO2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12OOH,gO2)
       0.00325825588012548_WP , & ! Ocoeffs(gO2C12H24OOH,gO2)
       0.00977076903085442_WP , & ! Ocoeffs(gN2,gHO2)
       0.0107882963000526_WP , & ! Ocoeffs(gO,gHO2)
       0.0156533054896757_WP , & ! Ocoeffs(gH2,gHO2)
       0.00801334531917_WP , & ! Ocoeffs(gH,gHO2)
       0.0107882963000526_WP , & ! Ocoeffs(gOH,gHO2)
       0.00403318410344248_WP , & ! Ocoeffs(gH2O,gHO2)
       0.00931098696461825_WP , & ! Ocoeffs(gH2O2,gHO2)
       0.00931098696461825_WP , & ! Ocoeffs(gO2,gHO2)
       0.00931098696461825_WP , & ! Ocoeffs(gHO2,gHO2)
       0.00432397513558173_WP , & ! Ocoeffs(gCH2O,gHO2)
       0.00617736049149457_WP , & ! Ocoeffs(gCO2,gHO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH3,gHO2)
       0.00974234165086526_WP , & ! Ocoeffs(gCO,gHO2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H6,gHO2)
       0.00811471281723581_WP , & ! Ocoeffs(gCH4,gHO2)
       0.0057583665640989_WP , & ! Ocoeffs(gC2H4,gHO2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H2,gHO2)
       0.00590751654601557_WP , & ! Ocoeffs(gC3H6,gHO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H81,gHO2)
       0.004556304729468_WP , & ! Ocoeffs(gC5H10,gHO2)
       0.00429548054975725_WP , & ! Ocoeffs(gC6H12,gHO2)
       0.00408508771298773_WP , & ! Ocoeffs(gC7H14,gHO2)
       0.00391175683751185_WP , & ! Ocoeffs(gC8H16,gHO2)
       0.00376918933864077_WP , & ! Ocoeffs(gC9H18,gHO2)
       0.00365201201114563_WP , & ! Ocoeffs(gC10H20,gHO2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12H25O2,gHO2)
       0.00343312653971418_WP , & ! Ocoeffs(gNXC12H26,gHO2)
       0.00333063337283927_WP , & ! Ocoeffs(gOC12H23OOH,gHO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2,gHO2)
       0.00432397513558173_WP , & ! Ocoeffs(gHCO,gHO2)
       0.00804112129610901_WP , & ! Ocoeffs(gCH2D,gHO2)
       0.00472530484840455_WP , & ! Ocoeffs(gCH3O,gHO2)
       0.00667459188746414_WP , & ! Ocoeffs(gC2H3,gHO2)
       0.00462119840403887_WP , & ! Ocoeffs(gCH2CHO,gHO2)
       0.00607490140898494_WP , & ! Ocoeffs(gC2H5,gHO2)
       0.00590751654601557_WP , & ! Ocoeffs(gAXC3H5,gHO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC2H3CHO,gHO2)
       0.00590751654601557_WP , & ! Ocoeffs(gNXC3H7,gHO2)
       0.00510697522799189_WP , & ! Ocoeffs(gC4H7,gHO2)
       0.00510697522799189_WP , & ! Ocoeffs(gPXC4H9,gHO2)
       0.00450794747859442_WP , & ! Ocoeffs(gPXC5H11,gHO2)
       0.00406299939117072_WP , & ! Ocoeffs(gPXC7H15,gHO2)
       0.00343312653971418_WP , & ! Ocoeffs(gPXC12H25,gHO2)
       0.00343312653971418_WP , & ! Ocoeffs(gS3XC12H25,gHO2)
       0.00343312653971418_WP , & ! Ocoeffs(gSXC12H25,gHO2)
       0.00340088194967515_WP , & ! Ocoeffs(gC12OOH,gHO2)
       0.00325825588012548_WP , & ! Ocoeffs(gO2C12H24OOH,gHO2)
       0.0045374955958451_WP , & ! Ocoeffs(gN2,gCH2O)
       0.00501003010035126_WP , & ! Ocoeffs(gO,gCH2O)
       0.00726931570028224_WP , & ! Ocoeffs(gH2,gCH2O)
       0.00372135693504775_WP , & ! Ocoeffs(gH,gCH2O)
       0.00501003010035126_WP , & ! Ocoeffs(gOH,gCH2O)
       0.00187299024762664_WP , & ! Ocoeffs(gH2O,gCH2O)
       0.00432397513558173_WP , & ! Ocoeffs(gH2O2,gCH2O)
       0.00432397513558173_WP , & ! Ocoeffs(gO2,gCH2O)
       0.00432397513558173_WP , & ! Ocoeffs(gHO2,gCH2O)
       0.00200803212851406_WP , & ! Ocoeffs(gCH2O,gCH2O)
       0.00286873489032348_WP , & ! Ocoeffs(gCO2,gCH2O)
       0.00373425595790184_WP , & ! Ocoeffs(gCH3,gCH2O)
       0.00452429406471742_WP , & ! Ocoeffs(gCO,gCH2O)
       0.00282115341191848_WP , & ! Ocoeffs(gC2H6,gCH2O)
       0.00376843148717184_WP , & ! Ocoeffs(gCH4,gCH2O)
       0.00267415623492388_WP , & ! Ocoeffs(gC2H4,gCH2O)
       0.00309964662943052_WP , & ! Ocoeffs(gC2H2,gCH2O)
       0.00274342073027016_WP , & ! Ocoeffs(gC3H6,gCH2O)
       0.0023716534012755_WP , & ! Ocoeffs(gC4H81,gCH2O)
       0.00211592481390192_WP , & ! Ocoeffs(gC5H10,gCH2O)
       0.00199479938733722_WP , & ! Ocoeffs(gC6H12,gCH2O)
       0.00189709402072539_WP , & ! Ocoeffs(gC7H14,gCH2O)
       0.00181660004101795_WP , & ! Ocoeffs(gC8H16,gCH2O)
       0.00175039241742195_WP , & ! Ocoeffs(gC9H18,gCH2O)
       0.00169597586067364_WP , & ! Ocoeffs(gC10H20,gCH2O)
       0.00157935233346629_WP , & ! Ocoeffs(gC12H25O2,gCH2O)
       0.00159432655758621_WP , & ! Ocoeffs(gNXC12H26,gCH2O)
       0.0015467292505748_WP , & ! Ocoeffs(gOC12H23OOH,gCH2O)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2,gCH2O)
       0.00200803212851406_WP , & ! Ocoeffs(gHCO,gCH2O)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2D,gCH2O)
       0.00219440761223134_WP , & ! Ocoeffs(gCH3O,gCH2O)
       0.00309964662943052_WP , & ! Ocoeffs(gC2H3,gCH2O)
       0.00214606110733324_WP , & ! Ocoeffs(gCH2CHO,gCH2O)
       0.00282115341191848_WP , & ! Ocoeffs(gC2H5,gCH2O)
       0.00274342073027016_WP , & ! Ocoeffs(gAXC3H5,gCH2O)
       0.0023716534012755_WP , & ! Ocoeffs(gC2H3CHO,gCH2O)
       0.00274342073027016_WP , & ! Ocoeffs(gNXC3H7,gCH2O)
       0.0023716534012755_WP , & ! Ocoeffs(gC4H7,gCH2O)
       0.0023716534012755_WP , & ! Ocoeffs(gPXC4H9,gCH2O)
       0.0020934679517887_WP , & ! Ocoeffs(gPXC5H11,gCH2O)
       0.00188683631607105_WP , & ! Ocoeffs(gPXC7H15,gCH2O)
       0.00159432655758621_WP , & ! Ocoeffs(gPXC12H25,gCH2O)
       0.00159432655758621_WP , & ! Ocoeffs(gS3XC12H25,gCH2O)
       0.00159432655758621_WP , & ! Ocoeffs(gSXC12H25,gCH2O)
       0.00157935233346629_WP , & ! Ocoeffs(gC12OOH,gCH2O)
       0.00151311750994414_WP , & ! Ocoeffs(gO2C12H24OOH,gCH2O)
       0.006482402221384_WP , & ! Ocoeffs(gN2,gCO2)
       0.00715747917892335_WP , & ! Ocoeffs(gO,gCO2)
       0.0103851623099316_WP , & ! Ocoeffs(gH2,gCO2)
       0.00531644206650152_WP , & ! Ocoeffs(gH,gCO2)
       0.00715747917892335_WP , & ! Ocoeffs(gOH,gCO2)
       0.00267581001135587_WP , & ! Ocoeffs(gH2O,gCO2)
       0.00617736049149457_WP , & ! Ocoeffs(gH2O2,gCO2)
       0.00617736049149457_WP , & ! Ocoeffs(gO2,gCO2)
       0.00617736049149457_WP , & ! Ocoeffs(gHO2,gCO2)
       0.00286873489032348_WP , & ! Ocoeffs(gCH2O,gCO2)
       0.00409836065573771_WP , & ! Ocoeffs(gCO2,gCO2)
       0.005334869997204_WP , & ! Ocoeffs(gCH3,gCO2)
       0.00646354211829406_WP , & ! Ocoeffs(gCO,gCO2)
       0.00403038432941538_WP , & ! Ocoeffs(gC2H6,gCO2)
       0.00538369418274364_WP , & ! Ocoeffs(gCH4,gCO2)
       0.00382037975606449_WP , & ! Ocoeffs(gC2H4,gCO2)
       0.00442824808789341_WP , & ! Ocoeffs(gC2H2,gCO2)
       0.00391933309034584_WP , & ! Ocoeffs(gC3H6,gCO2)
       0.00338821514027671_WP , & ! Ocoeffs(gC4H81,gCO2)
       0.00302287361479294_WP , & ! Ocoeffs(gC5H10,gCO2)
       0.00284983019962178_WP , & ! Ocoeffs(gC6H12,gCO2)
       0.00271024538412454_WP , & ! Ocoeffs(gC7H14,gCO2)
       0.00259524927187676_WP , & ! Ocoeffs(gC8H16,gCO2)
       0.00250066307620878_WP , & ! Ocoeffs(gC9H18,gCO2)
       0.00242292195208111_WP , & ! Ocoeffs(gC10H20,gCO2)
       0.00225631008527802_WP , & ! Ocoeffs(gC12H25O2,gCO2)
       0.0022777027107138_WP , & ! Ocoeffs(gNXC12H26,gCO2)
       0.00220970377116989_WP , & ! Ocoeffs(gOC12H23OOH,gCO2)
       0.005334869997204_WP , & ! Ocoeffs(gCH2,gCO2)
       0.00286873489032348_WP , & ! Ocoeffs(gHCO,gCO2)
       0.005334869997204_WP , & ! Ocoeffs(gCH2D,gCO2)
       0.00313499649303813_WP , & ! Ocoeffs(gCH3O,gCO2)
       0.00442824808789341_WP , & ! Ocoeffs(gC2H3,gCO2)
       0.00306592722693581_WP , & ! Ocoeffs(gCH2CHO,gCO2)
       0.00403038432941538_WP , & ! Ocoeffs(gC2H5,gCO2)
       0.00391933309034584_WP , & ! Ocoeffs(gAXC3H5,gCO2)
       0.00338821514027671_WP , & ! Ocoeffs(gC2H3CHO,gCO2)
       0.00391933309034584_WP , & ! Ocoeffs(gNXC3H7,gCO2)
       0.00338821514027671_WP , & ! Ocoeffs(gC4H7,gCO2)
       0.00338821514027671_WP , & ! Ocoeffs(gPXC4H9,gCO2)
       0.00299079106842499_WP , & ! Ocoeffs(gPXC5H11,gCO2)
       0.00269559091977674_WP , & ! Ocoeffs(gPXC7H15,gCO2)
       0.0022777027107138_WP , & ! Ocoeffs(gPXC12H25,gCO2)
       0.0022777027107138_WP , & ! Ocoeffs(gS3XC12H25,gCO2)
       0.0022777027107138_WP , & ! Ocoeffs(gSXC12H25,gCO2)
       0.00225631008527802_WP , & ! Ocoeffs(gC12OOH,gCO2)
       0.0021616850309802_WP , & ! Ocoeffs(gO2C12H24OOH,gCO2)
       0.00843819664144349_WP , & ! Ocoeffs(gN2,gCH3)
       0.00931694990624912_WP , & ! Ocoeffs(gO,gCH3)
       0.0135184517608969_WP , & ! Ocoeffs(gH2,gCH3)
       0.00692045665447833_WP , & ! Ocoeffs(gH,gCH3)
       0.00931694990624912_WP , & ! Ocoeffs(gOH,gCH3)
       0.00348312404566333_WP , & ! Ocoeffs(gH2O,gCH3)
       0.00804112129610901_WP , & ! Ocoeffs(gH2O2,gCH3)
       0.00804112129610901_WP , & ! Ocoeffs(gO2,gCH3)
       0.00804112129610901_WP , & ! Ocoeffs(gHO2,gCH3)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2O,gCH3)
       0.005334869997204_WP , & ! Ocoeffs(gCO2,gCH3)
       0.00694444444444444_WP , & ! Ocoeffs(gCH3,gCH3)
       0.00841364628910254_WP , & ! Ocoeffs(gCO,gCH3)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H6,gCH3)
       0.00700799929099234_WP , & ! Ocoeffs(gCH4,gCH3)
       0.00497301995860717_WP , & ! Ocoeffs(gC2H4,gCH3)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H2,gCH3)
       0.0051018283331073_WP , & ! Ocoeffs(gC3H6,gCH3)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H81,gCH3)
       0.00393489961170722_WP , & ! Ocoeffs(gC5H10,gCH3)
       0.0037096475654975_WP , & ! Ocoeffs(gC6H12,gCH3)
       0.00352794885549769_WP , & ! Ocoeffs(gC7H14,gCH3)
       0.0033782574640954_WP , & ! Ocoeffs(gC8H16,gCH3)
       0.00325513383008522_WP , & ! Ocoeffs(gC9H18,gCH3)
       0.00315393756516477_WP , & ! Ocoeffs(gC10H20,gCH3)
       0.00293705751871459_WP , & ! Ocoeffs(gC12H25O2,gCH3)
       0.0029649044763607_WP , & ! Ocoeffs(gNXC12H26,gCH3)
       0.00287638969377155_WP , & ! Ocoeffs(gOC12H23OOH,gCH3)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2,gCH3)
       0.00373425595790184_WP , & ! Ocoeffs(gHCO,gCH3)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2D,gCH3)
       0.00408085089061993_WP , & ! Ocoeffs(gCH3O,gCH3)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H3,gCH3)
       0.00399094285508813_WP , & ! Ocoeffs(gCH2CHO,gCH3)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H5,gCH3)
       0.0051018283331073_WP , & ! Ocoeffs(gAXC3H5,gCH3)
       0.00441046770020803_WP , & ! Ocoeffs(gC2H3CHO,gCH3)
       0.0051018283331073_WP , & ! Ocoeffs(gNXC3H7,gCH3)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H7,gCH3)
       0.00441046770020803_WP , & ! Ocoeffs(gPXC4H9,gCH3)
       0.00389313749547847_WP , & ! Ocoeffs(gPXC5H11,gCH3)
       0.0035088730179272_WP , & ! Ocoeffs(gPXC7H15,gCH3)
       0.0029649044763607_WP , & ! Ocoeffs(gPXC12H25,gCH3)
       0.0029649044763607_WP , & ! Ocoeffs(gS3XC12H25,gCH3)
       0.0029649044763607_WP , & ! Ocoeffs(gSXC12H25,gCH3)
       0.00293705751871459_WP , & ! Ocoeffs(gC12OOH,gCH3)
       0.00281388330210422_WP , & ! Ocoeffs(gO2C12H24OOH,gCH3)
       0.0102234242676958_WP , & ! Ocoeffs(gN2,gCO)
       0.0112880910246433_WP , & ! Ocoeffs(gO,gCO)
       0.0163784838949174_WP , & ! Ocoeffs(gH2,gCO)
       0.0083845835207779_WP , & ! Ocoeffs(gH,gCO)
       0.0112880910246433_WP , & ! Ocoeffs(gOH,gCO)
       0.00422003141298419_WP , & ! Ocoeffs(gH2O,gCO)
       0.00974234165086526_WP , & ! Ocoeffs(gH2O2,gCO)
       0.00974234165086526_WP , & ! Ocoeffs(gO2,gCO)
       0.00974234165086526_WP , & ! Ocoeffs(gHO2,gCO)
       0.00452429406471742_WP , & ! Ocoeffs(gCH2O,gCO)
       0.00646354211829406_WP , & ! Ocoeffs(gCO2,gCO)
       0.00841364628910254_WP , & ! Ocoeffs(gCH3,gCO)
       0.0101936799184506_WP , & ! Ocoeffs(gCO,gCO)
       0.00635633636332564_WP , & ! Ocoeffs(gC2H6,gCO)
       0.00849064712093149_WP , & ! Ocoeffs(gCH4,gCO)
       0.00602513725253301_WP , & ! Ocoeffs(gC2H4,gCO)
       0.00698380899843042_WP , & ! Ocoeffs(gC2H2,gCO)
       0.00618119697923804_WP , & ! Ocoeffs(gC3H6,gCO)
       0.00534356858866491_WP , & ! Ocoeffs(gC4H81,gCO)
       0.00476738690630853_WP , & ! Ocoeffs(gC5H10,gCO)
       0.00449447939615899_WP , & ! Ocoeffs(gC6H12,gCO)
       0.00427433958665305_WP , & ! Ocoeffs(gC7H14,gCO)
       0.00409297872620437_WP , & ! Ocoeffs(gC8H16,gCO)
       0.00394380643248412_WP , & ! Ocoeffs(gC9H18,gCO)
       0.00382120057313419_WP , & ! Ocoeffs(gC10H20,gCO)
       0.00355843628542278_WP , & ! Ocoeffs(gC12H25O2,gCO)
       0.0035921746864909_WP , & ! Ocoeffs(gNXC12H26,gCO)
       0.00348493326811399_WP , & ! Ocoeffs(gOC12H23OOH,gCO)
       0.00841364628910254_WP , & ! Ocoeffs(gCH2,gCO)
       0.00452429406471742_WP , & ! Ocoeffs(gHCO,gCO)
       0.00841364628910254_WP , & ! Ocoeffs(gCH2D,gCO)
       0.0049442163771233_WP , & ! Ocoeffs(gCH3O,gCO)
       0.00698380899843042_WP , & ! Ocoeffs(gC2H3,gCO)
       0.00483528694215349_WP , & ! Ocoeffs(gCH2CHO,gCO)
       0.00635633636332564_WP , & ! Ocoeffs(gC2H5,gCO)
       0.00618119697923804_WP , & ! Ocoeffs(gAXC3H5,gCO)
       0.00534356858866491_WP , & ! Ocoeffs(gC2H3CHO,gCO)
       0.00618119697923804_WP , & ! Ocoeffs(gNXC3H7,gCO)
       0.00534356858866491_WP , & ! Ocoeffs(gC4H7,gCO)
       0.00534356858866491_WP , & ! Ocoeffs(gPXC4H9,gCO)
       0.00471678938521896_WP , & ! Ocoeffs(gPXC5H11,gCO)
       0.00425122796825499_WP , & ! Ocoeffs(gPXC7H15,gCO)
       0.0035921746864909_WP , & ! Ocoeffs(gPXC12H25,gCO)
       0.0035921746864909_WP , & ! Ocoeffs(gS3XC12H25,gCO)
       0.0035921746864909_WP , & ! Ocoeffs(gSXC12H25,gCO)
       0.00355843628542278_WP , & ! Ocoeffs(gC12OOH,gCO)
       0.00340920270759122_WP , & ! Ocoeffs(gO2C12H24OOH,gCO)
       0.00637488364852818_WP , & ! Ocoeffs(gN2,gC2H6)
       0.00703876362868729_WP , & ! Ocoeffs(gO,gC2H6)
       0.0102129116855016_WP , & ! Ocoeffs(gH2,gC2H6)
       0.00522826236950968_WP , & ! Ocoeffs(gH,gC2H6)
       0.00703876362868729_WP , & ! Ocoeffs(gOH,gC2H6)
       0.0026314284281358_WP , & ! Ocoeffs(gH2O,gC2H6)
       0.00607490140898494_WP , & ! Ocoeffs(gH2O2,gC2H6)
       0.00607490140898494_WP , & ! Ocoeffs(gO2,gC2H6)
       0.00607490140898494_WP , & ! Ocoeffs(gHO2,gC2H6)
       0.00282115341191848_WP , & ! Ocoeffs(gCH2O,gC2H6)
       0.00403038432941538_WP , & ! Ocoeffs(gCO2,gC2H6)
       0.00524638465043263_WP , & ! Ocoeffs(gCH3,gC2H6)
       0.00635633636332564_WP , & ! Ocoeffs(gCO,gC2H6)
       0.00396353547364249_WP , & ! Ocoeffs(gC2H6,gC2H6)
       0.00529439902711271_WP , & ! Ocoeffs(gCH4,gC2H6)
       0.00375701408310697_WP , & ! Ocoeffs(gC2H4,gC2H6)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H2,gC2H6)
       0.00385432615525772_WP , & ! Ocoeffs(gC3H6,gC2H6)
       0.00333201744627844_WP , & ! Ocoeffs(gC4H81,gC2H6)
       0.00297273555703498_WP , & ! Ocoeffs(gC5H10,gC2H6)
       0.00280256227864428_WP , & ! Ocoeffs(gC6H12,gC2H6)
       0.0026652926481112_WP , & ! Ocoeffs(gC7H14,gC2H6)
       0.0025522038870969_WP , & ! Ocoeffs(gC8H16,gC2H6)
       0.00245918651922188_WP , & ! Ocoeffs(gC9H18,gC2H6)
       0.00238273482676367_WP , & ! Ocoeffs(gC10H20,gC2H6)
       0.00221888642164156_WP , & ! Ocoeffs(gC12H25O2,gC2H6)
       0.00223992422420799_WP , & ! Ocoeffs(gNXC12H26,gC2H6)
       0.00217305313028146_WP , & ! Ocoeffs(gOC12H23OOH,gC2H6)
       0.00524638465043263_WP , & ! Ocoeffs(gCH2,gC2H6)
       0.00282115341191848_WP , & ! Ocoeffs(gHCO,gC2H6)
       0.00524638465043263_WP , & ! Ocoeffs(gCH2D,gC2H6)
       0.00308299874014839_WP , & ! Ocoeffs(gCH3O,gC2H6)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H3,gC2H6)
       0.00301507507233909_WP , & ! Ocoeffs(gCH2CHO,gC2H6)
       0.00396353547364249_WP , & ! Ocoeffs(gC2H5,gC2H6)
       0.00385432615525772_WP , & ! Ocoeffs(gAXC3H5,gC2H6)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H3CHO,gC2H6)
       0.00385432615525772_WP , & ! Ocoeffs(gNXC3H7,gC2H6)
       0.00333201744627844_WP , & ! Ocoeffs(gC4H7,gC2H6)
       0.00333201744627844_WP , & ! Ocoeffs(gPXC4H9,gC2H6)
       0.00294118513895548_WP , & ! Ocoeffs(gPXC5H11,gC2H6)
       0.00265088124598615_WP , & ! Ocoeffs(gPXC7H15,gC2H6)
       0.00223992422420799_WP , & ! Ocoeffs(gPXC12H25,gC2H6)
       0.00223992422420799_WP , & ! Ocoeffs(gS3XC12H25,gC2H6)
       0.00223992422420799_WP , & ! Ocoeffs(gSXC12H25,gC2H6)
       0.00221888642164156_WP , & ! Ocoeffs(gC12OOH,gC2H6)
       0.00212583083965463_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H6)
       0.00851542215559055_WP , & ! Ocoeffs(gN2,gCH4)
       0.00940221768055752_WP , & ! Ocoeffs(gO,gCH4)
       0.0136421712512178_WP , & ! Ocoeffs(gH2,gCH4)
       0.00698379196722154_WP , & ! Ocoeffs(gH,gCH4)
       0.00940221768055752_WP , & ! Ocoeffs(gOH,gCH4)
       0.00351500124131236_WP , & ! Ocoeffs(gH2O,gCH4)
       0.00811471281723581_WP , & ! Ocoeffs(gH2O2,gCH4)
       0.00811471281723581_WP , & ! Ocoeffs(gO2,gCH4)
       0.00811471281723581_WP , & ! Ocoeffs(gHO2,gCH4)
       0.00376843148717184_WP , & ! Ocoeffs(gCH2O,gCH4)
       0.00538369418274364_WP , & ! Ocoeffs(gCO2,gCH4)
       0.00700799929099234_WP , & ! Ocoeffs(gCH3,gCH4)
       0.00849064712093149_WP , & ! Ocoeffs(gCO,gCH4)
       0.00529439902711271_WP , & ! Ocoeffs(gC2H6,gCH4)
       0.00707213578500707_WP , & ! Ocoeffs(gCH4,gCH4)
       0.00501853252953741_WP , & ! Ocoeffs(gC2H4,gCH4)
       0.00581704136681112_WP , & ! Ocoeffs(gC2H2,gCH4)
       0.00514851974513_WP , & ! Ocoeffs(gC3H6,gCH4)
       0.00445083185030435_WP , & ! Ocoeffs(gC4H81,gCH4)
       0.00397091141121171_WP , & ! Ocoeffs(gC5H10,gCH4)
       0.00374359788127266_WP , & ! Ocoeffs(gC6H12,gCH4)
       0.00356023628322985_WP , & ! Ocoeffs(gC7H14,gCH4)
       0.0034091749314965_WP , & ! Ocoeffs(gC8H16,gCH4)
       0.00328492448255843_WP , & ! Ocoeffs(gC9H18,gCH4)
       0.00318280207975327_WP , & ! Ocoeffs(gC10H20,gCH4)
       0.0029639371692608_WP , & ! Ocoeffs(gC12H25O2,gCH4)
       0.00299203897942019_WP , & ! Ocoeffs(gNXC12H26,gCH4)
       0.00290271411857789_WP , & ! Ocoeffs(gOC12H23OOH,gCH4)
       0.00700799929099234_WP , & ! Ocoeffs(gCH2,gCH4)
       0.00376843148717184_WP , & ! Ocoeffs(gHCO,gCH4)
       0.00700799929099234_WP , & ! Ocoeffs(gCH2D,gCH4)
       0.00411819842132783_WP , & ! Ocoeffs(gCH3O,gCH4)
       0.00581704136681112_WP , & ! Ocoeffs(gC2H3,gCH4)
       0.00402746755663419_WP , & ! Ocoeffs(gCH2CHO,gCH4)
       0.00529439902711271_WP , & ! Ocoeffs(gC2H5,gCH4)
       0.00514851974513_WP , & ! Ocoeffs(gAXC3H5,gCH4)
       0.00445083185030435_WP , & ! Ocoeffs(gC2H3CHO,gCH4)
       0.00514851974513_WP , & ! Ocoeffs(gNXC3H7,gCH4)
       0.00445083185030435_WP , & ! Ocoeffs(gC4H7,gCH4)
       0.00445083185030435_WP , & ! Ocoeffs(gPXC4H9,gCH4)
       0.00392876709235902_WP , & ! Ocoeffs(gPXC5H11,gCH4)
       0.0035409858655415_WP , & ! Ocoeffs(gPXC7H15,gCH4)
       0.00299203897942019_WP , & ! Ocoeffs(gPXC12H25,gCH4)
       0.00299203897942019_WP , & ! Ocoeffs(gS3XC12H25,gCH4)
       0.00299203897942019_WP , & ! Ocoeffs(gSXC12H25,gCH4)
       0.0029639371692608_WP , & ! Ocoeffs(gC12OOH,gCH4)
       0.00283963567479575_WP , & ! Ocoeffs(gO2C12H24OOH,gCH4)
       0.00604271812500727_WP , & ! Ocoeffs(gN2,gC2H4)
       0.0066720064085453_WP , & ! Ocoeffs(gO,gC2H4)
       0.00968076437996282_WP , & ! Ocoeffs(gH2,gC2H4)
       0.0049558419454171_WP , & ! Ocoeffs(gH,gC2H4)
       0.0066720064085453_WP , & ! Ocoeffs(gOH,gC2H4)
       0.00249431693722391_WP , & ! Ocoeffs(gH2O,gC2H4)
       0.0057583665640989_WP , & ! Ocoeffs(gH2O2,gC2H4)
       0.0057583665640989_WP , & ! Ocoeffs(gO2,gC2H4)
       0.0057583665640989_WP , & ! Ocoeffs(gHO2,gC2H4)
       0.00267415623492388_WP , & ! Ocoeffs(gCH2O,gC2H4)
       0.00382037975606449_WP , & ! Ocoeffs(gCO2,gC2H4)
       0.00497301995860717_WP , & ! Ocoeffs(gCH3,gC2H4)
       0.00602513725253301_WP , & ! Ocoeffs(gCO,gC2H4)
       0.00375701408310697_WP , & ! Ocoeffs(gC2H6,gC2H4)
       0.00501853252953741_WP , & ! Ocoeffs(gCH4,gC2H4)
       0.00356125356125356_WP , & ! Ocoeffs(gC2H4,gC2H4)
       0.0041278918013559_WP , & ! Ocoeffs(gC2H2,gC2H4)
       0.00365349515413394_WP , & ! Ocoeffs(gC3H6,gC2H4)
       0.00315840152158946_WP , & ! Ocoeffs(gC4H81,gC2H4)
       0.00281784013979553_WP , & ! Ocoeffs(gC5H10,gC2H4)
       0.00265653379909693_WP , & ! Ocoeffs(gC6H12,gC2H4)
       0.00252641665027229_WP , & ! Ocoeffs(gC7H14,gC2H4)
       0.00241922041837345_WP , & ! Ocoeffs(gC8H16,gC2H4)
       0.00233104975271297_WP , & ! Ocoeffs(gC9H18,gC2H4)
       0.0022585816022062_WP , & ! Ocoeffs(gC10H20,gC2H4)
       0.00210327057506087_WP , & ! Ocoeffs(gC12H25O2,gC2H4)
       0.00212321219562799_WP , & ! Ocoeffs(gNXC12H26,gC2H4)
       0.00205982544324354_WP , & ! Ocoeffs(gOC12H23OOH,gC2H4)
       0.00497301995860717_WP , & ! Ocoeffs(gCH2,gC2H4)
       0.00267415623492388_WP , & ! Ocoeffs(gHCO,gC2H4)
       0.00497301995860717_WP , & ! Ocoeffs(gCH2D,gC2H4)
       0.00292235802151_WP , & ! Ocoeffs(gCH3O,gC2H4)
       0.0041278918013559_WP , & ! Ocoeffs(gC2H3,gC2H4)
       0.00285797353997001_WP , & ! Ocoeffs(gCH2CHO,gC2H4)
       0.00375701408310697_WP , & ! Ocoeffs(gC2H5,gC2H4)
       0.00365349515413394_WP , & ! Ocoeffs(gAXC3H5,gC2H4)
       0.00315840152158946_WP , & ! Ocoeffs(gC2H3CHO,gC2H4)
       0.00365349515413394_WP , & ! Ocoeffs(gNXC3H7,gC2H4)
       0.00315840152158946_WP , & ! Ocoeffs(gC4H7,gC2H4)
       0.00315840152158946_WP , & ! Ocoeffs(gPXC4H9,gC2H4)
       0.00278793366719276_WP , & ! Ocoeffs(gPXC5H11,gC2H4)
       0.0025127561592533_WP , & ! Ocoeffs(gPXC7H15,gC2H4)
       0.00212321219562799_WP , & ! Ocoeffs(gPXC12H25,gC2H4)
       0.00212321219562799_WP , & ! Ocoeffs(gS3XC12H25,gC2H4)
       0.00212321219562799_WP , & ! Ocoeffs(gSXC12H25,gC2H4)
       0.00210327057506087_WP , & ! Ocoeffs(gC12OOH,gC2H4)
       0.00201506368644803_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H4)
       0.00700418719899912_WP , & ! Ocoeffs(gN2,gC2H2)
       0.00773360281112183_WP , & ! Ocoeffs(gO,gC2H2)
       0.0112210903345058_WP , & ! Ocoeffs(gH2,gC2H2)
       0.00574437595735306_WP , & ! Ocoeffs(gH,gC2H2)
       0.00773360281112183_WP , & ! Ocoeffs(gOH,gC2H2)
       0.00289119273819005_WP , & ! Ocoeffs(gH2O,gC2H2)
       0.00667459188746414_WP , & ! Ocoeffs(gH2O2,gC2H2)
       0.00667459188746414_WP , & ! Ocoeffs(gO2,gC2H2)
       0.00667459188746414_WP , & ! Ocoeffs(gHO2,gC2H2)
       0.00309964662943052_WP , & ! Ocoeffs(gCH2O,gC2H2)
       0.00442824808789341_WP , & ! Ocoeffs(gCO2,gC2H2)
       0.00576428719888396_WP , & ! Ocoeffs(gCH3,gC2H2)
       0.00698380899843042_WP , & ! Ocoeffs(gCO,gC2H2)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H6,gC2H2)
       0.00581704136681112_WP , & ! Ocoeffs(gCH4,gC2H2)
       0.0041278918013559_WP , & ! Ocoeffs(gC2H4,gC2H2)
       0.00478468899521531_WP , & ! Ocoeffs(gC2H2,gC2H2)
       0.00423481014020648_WP , & ! Ocoeffs(gC3H6,gC2H2)
       0.00366094116077764_WP , & ! Ocoeffs(gC4H81,gC2H2)
       0.00326619237033466_WP , & ! Ocoeffs(gC5H10,gC2H2)
       0.00307922025228023_WP , & ! Ocoeffs(gC6H12,gC2H2)
       0.00292839990135302_WP , & ! Ocoeffs(gC7H14,gC2H2)
       0.00280414746069398_WP , & ! Ocoeffs(gC8H16,gC2H2)
       0.00270194778250766_WP , & ! Ocoeffs(gC9H18,gC2H2)
       0.00261794907834603_WP , & ! Ocoeffs(gC10H20,gC2H2)
       0.00243792620028179_WP , & ! Ocoeffs(gC12H25O2,gC2H2)
       0.00246104077233596_WP , & ! Ocoeffs(gNXC12H26,gC2H2)
       0.00238756842587651_WP , & ! Ocoeffs(gOC12H23OOH,gC2H2)
       0.00576428719888396_WP , & ! Ocoeffs(gCH2,gC2H2)
       0.00309964662943052_WP , & ! Ocoeffs(gHCO,gC2H2)
       0.00576428719888396_WP , & ! Ocoeffs(gCH2D,gC2H2)
       0.00338734030310707_WP , & ! Ocoeffs(gCH3O,gC2H2)
       0.00478468899521531_WP , & ! Ocoeffs(gC2H3,gC2H2)
       0.00331271147679291_WP , & ! Ocoeffs(gCH2CHO,gC2H2)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H5,gC2H2)
       0.00423481014020648_WP , & ! Ocoeffs(gAXC3H5,gC2H2)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3CHO,gC2H2)
       0.00423481014020648_WP , & ! Ocoeffs(gNXC3H7,gC2H2)
       0.00366094116077764_WP , & ! Ocoeffs(gC4H7,gC2H2)
       0.00366094116077764_WP , & ! Ocoeffs(gPXC4H9,gC2H2)
       0.00323152741853016_WP , & ! Ocoeffs(gPXC5H11,gC2H2)
       0.00291256586204358_WP , & ! Ocoeffs(gPXC7H15,gC2H2)
       0.00246104077233596_WP , & ! Ocoeffs(gPXC12H25,gC2H2)
       0.00246104077233596_WP , & ! Ocoeffs(gS3XC12H25,gC2H2)
       0.00246104077233596_WP , & ! Ocoeffs(gSXC12H25,gC2H2)
       0.00243792620028179_WP , & ! Ocoeffs(gC12OOH,gC2H2)
       0.00233568453563607_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H2)
       0.00619923322161321_WP , & ! Ocoeffs(gN2,gC3H6)
       0.00684482097741742_WP , & ! Ocoeffs(gO,gC3H6)
       0.00993151011074227_WP , & ! Ocoeffs(gH2,gC3H6)
       0.00508420538465163_WP , & ! Ocoeffs(gH,gC3H6)
       0.00684482097741742_WP , & ! Ocoeffs(gOH,gC3H6)
       0.00255892333592052_WP , & ! Ocoeffs(gH2O,gC3H6)
       0.00590751654601557_WP , & ! Ocoeffs(gH2O2,gC3H6)
       0.00590751654601557_WP , & ! Ocoeffs(gO2,gC3H6)
       0.00590751654601557_WP , & ! Ocoeffs(gHO2,gC3H6)
       0.00274342073027016_WP , & ! Ocoeffs(gCH2O,gC3H6)
       0.00391933309034584_WP , & ! Ocoeffs(gCO2,gC3H6)
       0.0051018283331073_WP , & ! Ocoeffs(gCH3,gC3H6)
       0.00618119697923804_WP , & ! Ocoeffs(gCO,gC3H6)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H6,gC3H6)
       0.00514851974513_WP , & ! Ocoeffs(gCH4,gC3H6)
       0.00365349515413394_WP , & ! Ocoeffs(gC2H4,gC3H6)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H2,gC3H6)
       0.00374812593703148_WP , & ! Ocoeffs(gC3H6,gC3H6)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H81,gC3H6)
       0.00289082625507948_WP , & ! Ocoeffs(gC5H10,gC3H6)
       0.00272534184799167_WP , & ! Ocoeffs(gC6H12,gC3H6)
       0.00259185447773735_WP , & ! Ocoeffs(gC7H14,gC3H6)
       0.00248188170914685_WP , & ! Ocoeffs(gC8H16,gC3H6)
       0.00239142729634342_WP , & ! Ocoeffs(gC9H18,gC3H6)
       0.00231708211643651_WP , & ! Ocoeffs(gC10H20,gC3H6)
       0.00215774831015193_WP , & ! Ocoeffs(gC12H25O2,gC3H6)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC12H26,gC3H6)
       0.00211317788689078_WP , & ! Ocoeffs(gOC12H23OOH,gC3H6)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2,gC3H6)
       0.00274342073027016_WP , & ! Ocoeffs(gHCO,gC3H6)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2D,gC3H6)
       0.00299805130036093_WP , & ! Ocoeffs(gCH3O,gC3H6)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H3,gC3H6)
       0.00293199916808171_WP , & ! Ocoeffs(gCH2CHO,gC3H6)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H5,gC3H6)
       0.00374812593703148_WP , & ! Ocoeffs(gAXC3H5,gC3H6)
       0.00324020866682533_WP , & ! Ocoeffs(gC2H3CHO,gC3H6)
       0.00374812593703148_WP , & ! Ocoeffs(gNXC3H7,gC3H6)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H7,gC3H6)
       0.00324020866682533_WP , & ! Ocoeffs(gPXC4H9,gC3H6)
       0.00286014516179248_WP , & ! Ocoeffs(gPXC5H11,gC3H6)
       0.00257784016033969_WP , & ! Ocoeffs(gPXC7H15,gC3H6)
       0.00217820644739333_WP , & ! Ocoeffs(gPXC12H25,gC3H6)
       0.00217820644739333_WP , & ! Ocoeffs(gS3XC12H25,gC3H6)
       0.00217820644739333_WP , & ! Ocoeffs(gSXC12H25,gC3H6)
       0.00215774831015193_WP , & ! Ocoeffs(gC12OOH,gC3H6)
       0.00206725673616953_WP , & ! Ocoeffs(gO2C12H24OOH,gC3H6)
       0.00535916069785301_WP , & ! Ocoeffs(gN2,gC4H81)
       0.00591726335413939_WP , & ! Ocoeffs(gO,gC4H81)
       0.00858566805814886_WP , & ! Ocoeffs(gH2,gC4H81)
       0.00439523287851836_WP , & ! Ocoeffs(gH,gC4H81)
       0.00591726335413939_WP , & ! Ocoeffs(gOH,gC4H81)
       0.00221215767828711_WP , & ! Ocoeffs(gH2O,gC4H81)
       0.00510697522799189_WP , & ! Ocoeffs(gH2O2,gC4H81)
       0.00510697522799189_WP , & ! Ocoeffs(gO2,gC4H81)
       0.00510697522799189_WP , & ! Ocoeffs(gHO2,gC4H81)
       0.0023716534012755_WP , & ! Ocoeffs(gCH2O,gC4H81)
       0.00338821514027671_WP , & ! Ocoeffs(gCO2,gC4H81)
       0.00441046770020803_WP , & ! Ocoeffs(gCH3,gC4H81)
       0.00534356858866491_WP , & ! Ocoeffs(gCO,gC4H81)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H6,gC4H81)
       0.00445083185030435_WP , & ! Ocoeffs(gCH4,gC4H81)
       0.00315840152158946_WP , & ! Ocoeffs(gC2H4,gC4H81)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H2,gC4H81)
       0.00324020866682533_WP , & ! Ocoeffs(gC3H6,gC4H81)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H81,gC4H81)
       0.00249908366030339_WP , & ! Ocoeffs(gC5H10,gC4H81)
       0.00235602443041662_WP , & ! Ocoeffs(gC6H12,gC4H81)
       0.00224062624442282_WP , & ! Ocoeffs(gC7H14,gC4H81)
       0.00214555614168664_WP , & ! Ocoeffs(gC8H16,gC4H81)
       0.00206735941691212_WP , & ! Ocoeffs(gC9H18,gC4H81)
       0.002003088925387_WP , & ! Ocoeffs(gC10H20,gC4H81)
       0.00186534681407191_WP , & ! Ocoeffs(gC12H25O2,gC4H81)
       0.00188303262151551_WP , & ! Ocoeffs(gNXC12H26,gC4H81)
       0.00182681623261306_WP , & ! Ocoeffs(gOC12H23OOH,gC4H81)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2,gC4H81)
       0.0023716534012755_WP , & ! Ocoeffs(gHCO,gC4H81)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2D,gC4H81)
       0.00259177839011199_WP , & ! Ocoeffs(gCH3O,gC4H81)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3,gC4H81)
       0.00253467713602688_WP , & ! Ocoeffs(gCH2CHO,gC4H81)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H5,gC4H81)
       0.00324020866682533_WP , & ! Ocoeffs(gAXC3H5,gC4H81)
       0.00280112044817927_WP , & ! Ocoeffs(gC2H3CHO,gC4H81)
       0.00324020866682533_WP , & ! Ocoeffs(gNXC3H7,gC4H81)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H7,gC4H81)
       0.00280112044817927_WP , & ! Ocoeffs(gPXC4H9,gC4H81)
       0.00247256023338382_WP , & ! Ocoeffs(gPXC5H11,gC4H81)
       0.00222851103979671_WP , & ! Ocoeffs(gPXC7H15,gC4H81)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC12H25,gC4H81)
       0.00188303262151551_WP , & ! Ocoeffs(gS3XC12H25,gC4H81)
       0.00188303262151551_WP , & ! Ocoeffs(gSXC12H25,gC4H81)
       0.00186534681407191_WP , & ! Ocoeffs(gC12OOH,gC4H81)
       0.0017871179639163_WP , & ! Ocoeffs(gO2C12H24OOH,gC4H81)
       0.00478129776306122_WP , & ! Ocoeffs(gN2,gC5H10)
       0.00527922180984892_WP , & ! Ocoeffs(gO,gC5H10)
       0.00765990008421637_WP , & ! Ocoeffs(gH2,gC5H10)
       0.00392130751716625_WP , & ! Ocoeffs(gH,gC5H10)
       0.00527922180984892_WP , & ! Ocoeffs(gOH,gC5H10)
       0.00197362705749245_WP , & ! Ocoeffs(gH2O,gC5H10)
       0.004556304729468_WP , & ! Ocoeffs(gH2O2,gC5H10)
       0.004556304729468_WP , & ! Ocoeffs(gO2,gC5H10)
       0.004556304729468_WP , & ! Ocoeffs(gHO2,gC5H10)
       0.00211592481390192_WP , & ! Ocoeffs(gCH2O,gC5H10)
       0.00302287361479294_WP , & ! Ocoeffs(gCO2,gC5H10)
       0.00393489961170722_WP , & ! Ocoeffs(gCH3,gC5H10)
       0.00476738690630853_WP , & ! Ocoeffs(gCO,gC5H10)
       0.00297273555703498_WP , & ! Ocoeffs(gC2H6,gC5H10)
       0.00397091141121171_WP , & ! Ocoeffs(gCH4,gC5H10)
       0.00281784013979553_WP , & ! Ocoeffs(gC2H4,gC5H10)
       0.00326619237033466_WP , & ! Ocoeffs(gC2H2,gC5H10)
       0.00289082625507948_WP , & ! Ocoeffs(gC3H6,gC5H10)
       0.00249908366030339_WP , & ! Ocoeffs(gC4H81,gC5H10)
       0.00222961463340676_WP , & ! Ocoeffs(gC5H10,gC5H10)
       0.00210198107016673_WP , & ! Ocoeffs(gC6H12,gC5H10)
       0.0019990259397534_WP , & ! Ocoeffs(gC7H14,gC5H10)
       0.00191420697365511_WP , & ! Ocoeffs(gC8H16,gC5H10)
       0.00184444197754426_WP , & ! Ocoeffs(gC9H18,gC5H10)
       0.00178710158887426_WP , & ! Ocoeffs(gC10H20,gC5H10)
       0.00166421181455306_WP , & ! Ocoeffs(gC12H25O2,gC5H10)
       0.00167999061208042_WP , & ! Ocoeffs(gNXC12H26,gC5H10)
       0.00162983587523621_WP , & ! Ocoeffs(gOC12H23OOH,gC5H10)
       0.00393489961170722_WP , & ! Ocoeffs(gCH2,gC5H10)
       0.00211592481390192_WP , & ! Ocoeffs(gHCO,gC5H10)
       0.00393489961170722_WP , & ! Ocoeffs(gCH2D,gC5H10)
       0.0023123143562307_WP , & ! Ocoeffs(gCH3O,gC5H10)
       0.00326619237033466_WP , & ! Ocoeffs(gC2H3,gC5H10)
       0.00226137016667981_WP , & ! Ocoeffs(gCH2CHO,gC5H10)
       0.00297273555703498_WP , & ! Ocoeffs(gC2H5,gC5H10)
       0.00289082625507948_WP , & ! Ocoeffs(gAXC3H5,gC5H10)
       0.00249908366030339_WP , & ! Ocoeffs(gC2H3CHO,gC5H10)
       0.00289082625507948_WP , & ! Ocoeffs(gNXC3H7,gC5H10)
       0.00249908366030339_WP , & ! Ocoeffs(gC4H7,gC5H10)
       0.00249908366030339_WP , & ! Ocoeffs(gPXC4H9,gC5H10)
       0.00220595115157647_WP , & ! Ocoeffs(gPXC5H11,gC5H10)
       0.00198821708291112_WP , & ! Ocoeffs(gPXC7H15,gC5H10)
       0.00167999061208042_WP , & ! Ocoeffs(gPXC12H25,gC5H10)
       0.00167999061208042_WP , & ! Ocoeffs(gS3XC12H25,gC5H10)
       0.00167999061208042_WP , & ! Ocoeffs(gSXC12H25,gC5H10)
       0.00166421181455306_WP , & ! Ocoeffs(gC12OOH,gC5H10)
       0.00159441815704887_WP , & ! Ocoeffs(gO2C12H24OOH,gC5H10)
       0.00450759393044929_WP , & ! Ocoeffs(gN2,gC6H12)
       0.00497701447741141_WP , & ! Ocoeffs(gO,gC6H12)
       0.00722141159963134_WP , & ! Ocoeffs(gH2,gC6H12)
       0.00369683354597998_WP , & ! Ocoeffs(gH,gC6H12)
       0.00497701447741141_WP , & ! Ocoeffs(gOH,gC6H12)
       0.00186064741963019_WP , & ! Ocoeffs(gH2O,gC6H12)
       0.00429548054975725_WP , & ! Ocoeffs(gH2O2,gC6H12)
       0.00429548054975725_WP , & ! Ocoeffs(gO2,gC6H12)
       0.00429548054975725_WP , & ! Ocoeffs(gHO2,gC6H12)
       0.00199479938733722_WP , & ! Ocoeffs(gCH2O,gC6H12)
       0.00284983019962178_WP , & ! Ocoeffs(gCO2,gC6H12)
       0.0037096475654975_WP , & ! Ocoeffs(gCH3,gC6H12)
       0.00449447939615899_WP , & ! Ocoeffs(gCO,gC6H12)
       0.00280256227864428_WP , & ! Ocoeffs(gC2H6,gC6H12)
       0.00374359788127266_WP , & ! Ocoeffs(gCH4,gC6H12)
       0.00265653379909693_WP , & ! Ocoeffs(gC2H4,gC6H12)
       0.00307922025228023_WP , & ! Ocoeffs(gC2H2,gC6H12)
       0.00272534184799167_WP , & ! Ocoeffs(gC3H6,gC6H12)
       0.00235602443041662_WP , & ! Ocoeffs(gC4H81,gC6H12)
       0.00210198107016673_WP , & ! Ocoeffs(gC5H10,gC6H12)
       0.00198165384866902_WP , & ! Ocoeffs(gC6H12,gC6H12)
       0.00188459235115153_WP , & ! Ocoeffs(gC7H14,gC6H12)
       0.00180462881913196_WP , & ! Ocoeffs(gC8H16,gC6H12)
       0.00173885749749276_WP , & ! Ocoeffs(gC9H18,gC6H12)
       0.00168479954068962_WP , & ! Ocoeffs(gC10H20,gC6H12)
       0.0015689445514597_WP , & ! Ocoeffs(gC12H25O2,gC6H12)
       0.00158382009686363_WP , & ! Ocoeffs(gNXC12H26,gC6H12)
       0.00153653645158873_WP , & ! Ocoeffs(gOC12H23OOH,gC6H12)
       0.0037096475654975_WP , & ! Ocoeffs(gCH2,gC6H12)
       0.00199479938733722_WP , & ! Ocoeffs(gHCO,gC6H12)
       0.0037096475654975_WP , & ! Ocoeffs(gCH2D,gC6H12)
       0.0021799466743027_WP , & ! Ocoeffs(gCH3O,gC6H12)
       0.00307922025228023_WP , & ! Ocoeffs(gC2H3,gC6H12)
       0.0021319187682841_WP , & ! Ocoeffs(gCH2CHO,gC6H12)
       0.00280256227864428_WP , & ! Ocoeffs(gC2H5,gC6H12)
       0.00272534184799167_WP , & ! Ocoeffs(gAXC3H5,gC6H12)
       0.00235602443041662_WP , & ! Ocoeffs(gC2H3CHO,gC6H12)
       0.00272534184799167_WP , & ! Ocoeffs(gNXC3H7,gC6H12)
       0.00235602443041662_WP , & ! Ocoeffs(gC4H7,gC6H12)
       0.00235602443041662_WP , & ! Ocoeffs(gPXC4H9,gC6H12)
       0.00207967219664382_WP , & ! Ocoeffs(gPXC5H11,gC6H12)
       0.00187440224379746_WP , & ! Ocoeffs(gPXC7H15,gC6H12)
       0.00158382009686363_WP , & ! Ocoeffs(gPXC12H25,gC6H12)
       0.00158382009686363_WP , & ! Ocoeffs(gS3XC12H25,gC6H12)
       0.00158382009686363_WP , & ! Ocoeffs(gSXC12H25,gC6H12)
       0.0015689445514597_WP , & ! Ocoeffs(gC12OOH,gC6H12)
       0.00150314620913928_WP , & ! Ocoeffs(gO2C12H24OOH,gC6H12)
       0.00428681177044489_WP , & ! Ocoeffs(gN2,gC7H14)
       0.00473324007722126_WP , & ! Ocoeffs(gO,gC7H14)
       0.0068677065241858_WP , & ! Ocoeffs(gH2,gC7H14)
       0.0035157624672511_WP , & ! Ocoeffs(gH,gC7H14)
       0.00473324007722126_WP , & ! Ocoeffs(gOH,gC7H14)
       0.00176951282262544_WP , & ! Ocoeffs(gH2O,gC7H14)
       0.00408508771298773_WP , & ! Ocoeffs(gH2O2,gC7H14)
       0.00408508771298773_WP , & ! Ocoeffs(gO2,gC7H14)
       0.00408508771298773_WP , & ! Ocoeffs(gHO2,gC7H14)
       0.00189709402072539_WP , & ! Ocoeffs(gCH2O,gC7H14)
       0.00271024538412454_WP , & ! Ocoeffs(gCO2,gC7H14)
       0.00352794885549769_WP , & ! Ocoeffs(gCH3,gC7H14)
       0.00427433958665305_WP , & ! Ocoeffs(gCO,gC7H14)
       0.0026652926481112_WP , & ! Ocoeffs(gC2H6,gC7H14)
       0.00356023628322985_WP , & ! Ocoeffs(gCH4,gC7H14)
       0.00252641665027229_WP , & ! Ocoeffs(gC2H4,gC7H14)
       0.00292839990135302_WP , & ! Ocoeffs(gC2H2,gC7H14)
       0.00259185447773735_WP , & ! Ocoeffs(gC3H6,gC7H14)
       0.00224062624442282_WP , & ! Ocoeffs(gC4H81,gC7H14)
       0.0019990259397534_WP , & ! Ocoeffs(gC5H10,gC7H14)
       0.00188459235115153_WP , & ! Ocoeffs(gC6H12,gC7H14)
       0.00179228493028908_WP , & ! Ocoeffs(gC7H14,gC7H14)
       0.0017162380157806_WP , & ! Ocoeffs(gC8H16,gC7H14)
       0.0016536881765291_WP , & ! Ocoeffs(gC9H18,gC7H14)
       0.00160227798096013_WP , & ! Ocoeffs(gC10H20,gC7H14)
       0.00149209757448193_WP , & ! Ocoeffs(gC12H25O2,gC7H14)
       0.00150624451498129_WP , & ! Ocoeffs(gNXC12H26,gC7H14)
       0.00146127682484737_WP , & ! Ocoeffs(gOC12H23OOH,gC7H14)
       0.00352794885549769_WP , & ! Ocoeffs(gCH2,gC7H14)
       0.00189709402072539_WP , & ! Ocoeffs(gHCO,gC7H14)
       0.00352794885549769_WP , & ! Ocoeffs(gCH2D,gC7H14)
       0.00207317278497878_WP , & ! Ocoeffs(gCH3O,gC7H14)
       0.00292839990135302_WP , & ! Ocoeffs(gC2H3,gC7H14)
       0.00202749728802694_WP , & ! Ocoeffs(gCH2CHO,gC7H14)
       0.0026652926481112_WP , & ! Ocoeffs(gC2H5,gC7H14)
       0.00259185447773735_WP , & ! Ocoeffs(gAXC3H5,gC7H14)
       0.00224062624442282_WP , & ! Ocoeffs(gC2H3CHO,gC7H14)
       0.00259185447773735_WP , & ! Ocoeffs(gNXC3H7,gC7H14)
       0.00224062624442282_WP , & ! Ocoeffs(gC4H7,gC7H14)
       0.00224062624442282_WP , & ! Ocoeffs(gPXC4H9,gC7H14)
       0.00197780975589145_WP , & ! Ocoeffs(gPXC5H11,gC7H14)
       0.00178259393486635_WP , & ! Ocoeffs(gPXC7H15,gC7H14)
       0.00150624451498129_WP , & ! Ocoeffs(gPXC12H25,gC7H14)
       0.00150624451498129_WP , & ! Ocoeffs(gS3XC12H25,gC7H14)
       0.00150624451498129_WP , & ! Ocoeffs(gSXC12H25,gC7H14)
       0.00149209757448193_WP , & ! Ocoeffs(gC12OOH,gC7H14)
       0.00142952203802342_WP , & ! Ocoeffs(gO2C12H24OOH,gC7H14)
       0.00410492171339442_WP , & ! Ocoeffs(gN2,gC8H16)
       0.00453240800112803_WP , & ! Ocoeffs(gO,gC8H16)
       0.00657630872125395_WP , & ! Ocoeffs(gH2,gC8H16)
       0.00336658814610338_WP , & ! Ocoeffs(gH,gC8H16)
       0.00453240800112803_WP , & ! Ocoeffs(gOH,gC8H16)
       0.00169443213201105_WP , & ! Ocoeffs(gH2O,gC8H16)
       0.00391175683751185_WP , & ! Ocoeffs(gH2O2,gC8H16)
       0.00391175683751185_WP , & ! Ocoeffs(gO2,gC8H16)
       0.00391175683751185_WP , & ! Ocoeffs(gHO2,gC8H16)
       0.00181660004101795_WP , & ! Ocoeffs(gCH2O,gC8H16)
       0.00259524927187676_WP , & ! Ocoeffs(gCO2,gC8H16)
       0.0033782574640954_WP , & ! Ocoeffs(gCH3,gC8H16)
       0.00409297872620437_WP , & ! Ocoeffs(gCO,gC8H16)
       0.0025522038870969_WP , & ! Ocoeffs(gC2H6,gC8H16)
       0.0034091749314965_WP , & ! Ocoeffs(gCH4,gC8H16)
       0.00241922041837345_WP , & ! Ocoeffs(gC2H4,gC8H16)
       0.00280414746069398_WP , & ! Ocoeffs(gC2H2,gC8H16)
       0.00248188170914685_WP , & ! Ocoeffs(gC3H6,gC8H16)
       0.00214555614168664_WP , & ! Ocoeffs(gC4H81,gC8H16)
       0.00191420697365511_WP , & ! Ocoeffs(gC5H10,gC8H16)
       0.00180462881913196_WP , & ! Ocoeffs(gC6H12,gC8H16)
       0.0017162380157806_WP , & ! Ocoeffs(gC7H14,gC8H16)
       0.00164341778309515_WP , & ! Ocoeffs(gC8H16,gC8H16)
       0.00158352194276854_WP , & ! Ocoeffs(gC9H18,gC8H16)
       0.00153429308939646_WP , & ! Ocoeffs(gC10H20,gC8H16)
       0.00142878765385081_WP , & ! Ocoeffs(gC12H25O2,gC8H16)
       0.00144233433757374_WP , & ! Ocoeffs(gNXC12H26,gC8H16)
       0.00139927463317884_WP , & ! Ocoeffs(gOC12H23OOH,gC8H16)
       0.0033782574640954_WP , & ! Ocoeffs(gCH2,gC8H16)
       0.00181660004101795_WP , & ! Ocoeffs(gHCO,gC8H16)
       0.0033782574640954_WP , & ! Ocoeffs(gCH2D,gC8H16)
       0.00198520775727799_WP , & ! Ocoeffs(gCH3O,gC8H16)
       0.00280414746069398_WP , & ! Ocoeffs(gC2H3,gC8H16)
       0.00194147027841308_WP , & ! Ocoeffs(gCH2CHO,gC8H16)
       0.0025522038870969_WP , & ! Ocoeffs(gC2H5,gC8H16)
       0.00248188170914685_WP , & ! Ocoeffs(gAXC3H5,gC8H16)
       0.00214555614168664_WP , & ! Ocoeffs(gC2H3CHO,gC8H16)
       0.00248188170914685_WP , & ! Ocoeffs(gNXC3H7,gC8H16)
       0.00214555614168664_WP , & ! Ocoeffs(gC4H7,gC8H16)
       0.00214555614168664_WP , & ! Ocoeffs(gPXC4H9,gC8H16)
       0.00189389099561037_WP , & ! Ocoeffs(gPXC5H11,gC8H16)
       0.00170695821072608_WP , & ! Ocoeffs(gPXC7H15,gC8H16)
       0.00144233433757374_WP , & ! Ocoeffs(gPXC12H25,gC8H16)
       0.00144233433757374_WP , & ! Ocoeffs(gS3XC12H25,gC8H16)
       0.00144233433757374_WP , & ! Ocoeffs(gSXC12H25,gC8H16)
       0.00142878765385081_WP , & ! Ocoeffs(gC12OOH,gC8H16)
       0.00136886720665349_WP , & ! Ocoeffs(gO2C12H24OOH,gC8H16)
       0.00395531414675628_WP , & ! Ocoeffs(gN2,gC9H18)
       0.00436722031195788_WP , & ! Ocoeffs(gO,gC9H18)
       0.00633662923064696_WP , & ! Ocoeffs(gH2,gC9H18)
       0.00324388981089084_WP , & ! Ocoeffs(gH,gC9H18)
       0.00436722031195788_WP , & ! Ocoeffs(gOH,gC9H18)
       0.00163267702782077_WP , & ! Ocoeffs(gH2O,gC9H18)
       0.00376918933864077_WP , & ! Ocoeffs(gH2O2,gC9H18)
       0.00376918933864077_WP , & ! Ocoeffs(gO2,gC9H18)
       0.00376918933864077_WP , & ! Ocoeffs(gHO2,gC9H18)
       0.00175039241742195_WP , & ! Ocoeffs(gCH2O,gC9H18)
       0.00250066307620878_WP , & ! Ocoeffs(gCO2,gC9H18)
       0.00325513383008522_WP , & ! Ocoeffs(gCH3,gC9H18)
       0.00394380643248412_WP , & ! Ocoeffs(gCO,gC9H18)
       0.00245918651922188_WP , & ! Ocoeffs(gC2H6,gC9H18)
       0.00328492448255843_WP , & ! Ocoeffs(gCH4,gC9H18)
       0.00233104975271297_WP , & ! Ocoeffs(gC2H4,gC9H18)
       0.00270194778250766_WP , & ! Ocoeffs(gC2H2,gC9H18)
       0.00239142729634342_WP , & ! Ocoeffs(gC3H6,gC9H18)
       0.00206735941691212_WP , & ! Ocoeffs(gC4H81,gC9H18)
       0.00184444197754426_WP , & ! Ocoeffs(gC5H10,gC9H18)
       0.00173885749749276_WP , & ! Ocoeffs(gC6H12,gC9H18)
       0.0016536881765291_WP , & ! Ocoeffs(gC7H14,gC9H18)
       0.00158352194276854_WP , & ! Ocoeffs(gC8H16,gC9H18)
       0.0015258090602542_WP , & ! Ocoeffs(gC9H18,gC9H18)
       0.0014783743967536_WP , & ! Ocoeffs(gC10H20,gC9H18)
       0.00137671420177066_WP , & ! Ocoeffs(gC12H25O2,gC9H18)
       0.00138976716441208_WP , & ! Ocoeffs(gNXC12H26,gC9H18)
       0.00134827680970141_WP , & ! Ocoeffs(gOC12H23OOH,gC9H18)
       0.00325513383008522_WP , & ! Ocoeffs(gCH2,gC9H18)
       0.00175039241742195_WP , & ! Ocoeffs(gHCO,gC9H18)
       0.00325513383008522_WP , & ! Ocoeffs(gCH2D,gC9H18)
       0.00191285507370101_WP , & ! Ocoeffs(gCH3O,gC9H18)
       0.00270194778250766_WP , & ! Ocoeffs(gC2H3,gC9H18)
       0.00187071164662094_WP , & ! Ocoeffs(gCH2CHO,gC9H18)
       0.00245918651922188_WP , & ! Ocoeffs(gC2H5,gC9H18)
       0.00239142729634342_WP , & ! Ocoeffs(gAXC3H5,gC9H18)
       0.00206735941691212_WP , & ! Ocoeffs(gC2H3CHO,gC9H18)
       0.00239142729634342_WP , & ! Ocoeffs(gNXC3H7,gC9H18)
       0.00206735941691212_WP , & ! Ocoeffs(gC4H7,gC9H18)
       0.00206735941691212_WP , & ! Ocoeffs(gPXC4H9,gC9H18)
       0.00182486643360555_WP , & ! Ocoeffs(gPXC5H11,gC9H18)
       0.00164474658232244_WP , & ! Ocoeffs(gPXC7H15,gC9H18)
       0.00138976716441208_WP , & ! Ocoeffs(gPXC12H25,gC9H18)
       0.00138976716441208_WP , & ! Ocoeffs(gS3XC12H25,gC9H18)
       0.00138976716441208_WP , & ! Ocoeffs(gSXC12H25,gC9H18)
       0.00137671420177066_WP , & ! Ocoeffs(gC12OOH,gC9H18)
       0.00131897760920516_WP , & ! Ocoeffs(gO2C12H24OOH,gC9H18)
       0.0038323505332362_WP , & ! Ocoeffs(gN2,gC10H20)
       0.00423145127549916_WP , & ! Ocoeffs(gO,gC10H20)
       0.00613963480774471_WP , & ! Ocoeffs(gH2,gC10H20)
       0.00314304310233413_WP , & ! Ocoeffs(gH,gC10H20)
       0.00423145127549916_WP , & ! Ocoeffs(gOH,gC10H20)
       0.00158192003113146_WP , & ! Ocoeffs(gH2O,gC10H20)
       0.00365201201114563_WP , & ! Ocoeffs(gH2O2,gC10H20)
       0.00365201201114563_WP , & ! Ocoeffs(gO2,gC10H20)
       0.00365201201114563_WP , & ! Ocoeffs(gHO2,gC10H20)
       0.00169597586067364_WP , & ! Ocoeffs(gCH2O,gC10H20)
       0.00242292195208111_WP , & ! Ocoeffs(gCO2,gC10H20)
       0.00315393756516477_WP , & ! Ocoeffs(gCH3,gC10H20)
       0.00382120057313419_WP , & ! Ocoeffs(gCO,gC10H20)
       0.00238273482676367_WP , & ! Ocoeffs(gC2H6,gC10H20)
       0.00318280207975327_WP , & ! Ocoeffs(gCH4,gC10H20)
       0.0022585816022062_WP , & ! Ocoeffs(gC2H4,gC10H20)
       0.00261794907834603_WP , & ! Ocoeffs(gC2H2,gC10H20)
       0.00231708211643651_WP , & ! Ocoeffs(gC3H6,gC10H20)
       0.002003088925387_WP , & ! Ocoeffs(gC4H81,gC10H20)
       0.00178710158887426_WP , & ! Ocoeffs(gC5H10,gC10H20)
       0.00168479954068962_WP , & ! Ocoeffs(gC6H12,gC10H20)
       0.00160227798096013_WP , & ! Ocoeffs(gC7H14,gC10H20)
       0.00153429308939646_WP , & ! Ocoeffs(gC8H16,gC10H20)
       0.0014783743967536_WP , & ! Ocoeffs(gC9H18,gC10H20)
       0.00143241439175388_WP , & ! Ocoeffs(gC10H20,gC10H20)
       0.0013339146296626_WP , & ! Ocoeffs(gC12H25O2,gC10H20)
       0.00134656179913716_WP , & ! Ocoeffs(gNXC12H26,gC10H20)
       0.0013063613050424_WP , & ! Ocoeffs(gOC12H23OOH,gC10H20)
       0.00315393756516477_WP , & ! Ocoeffs(gCH2,gC10H20)
       0.00169597586067364_WP , & ! Ocoeffs(gHCO,gC10H20)
       0.00315393756516477_WP , & ! Ocoeffs(gCH2D,gC10H20)
       0.00185338784473377_WP , & ! Ocoeffs(gCH3O,gC10H20)
       0.00261794907834603_WP , & ! Ocoeffs(gC2H3,gC10H20)
       0.00181255458111673_WP , & ! Ocoeffs(gCH2CHO,gC10H20)
       0.00238273482676367_WP , & ! Ocoeffs(gC2H5,gC10H20)
       0.00231708211643651_WP , & ! Ocoeffs(gAXC3H5,gC10H20)
       0.002003088925387_WP , & ! Ocoeffs(gC2H3CHO,gC10H20)
       0.00231708211643651_WP , & ! Ocoeffs(gNXC3H7,gC10H20)
       0.002003088925387_WP , & ! Ocoeffs(gC4H7,gC10H20)
       0.002003088925387_WP , & ! Ocoeffs(gPXC4H9,gC10H20)
       0.00176813461344111_WP , & ! Ocoeffs(gPXC5H11,gC10H20)
       0.00159361436485925_WP , & ! Ocoeffs(gPXC7H15,gC10H20)
       0.00134656179913716_WP , & ! Ocoeffs(gPXC12H25,gC10H20)
       0.00134656179913716_WP , & ! Ocoeffs(gS3XC12H25,gC10H20)
       0.00134656179913716_WP , & ! Ocoeffs(gSXC12H25,gC10H20)
       0.0013339146296626_WP , & ! Ocoeffs(gC12OOH,gC10H20)
       0.00127797296407148_WP , & ! Ocoeffs(gO2C12H24OOH,gC10H20)
       0.00356881952018071_WP , & ! Ocoeffs(gN2,gC12H25O2)
       0.00394047615940362_WP , & ! Ocoeffs(gO,gC12H25O2)
       0.00571744373554407_WP , & ! Ocoeffs(gH2,gC12H25O2)
       0.00292691221199626_WP , & ! Ocoeffs(gH,gC12H25O2)
       0.00394047615940362_WP , & ! Ocoeffs(gOH,gC12H25O2)
       0.00147313953603807_WP , & ! Ocoeffs(gH2O,gC12H25O2)
       0.00340088194967515_WP , & ! Ocoeffs(gH2O2,gC12H25O2)
       0.00340088194967515_WP , & ! Ocoeffs(gO2,gC12H25O2)
       0.00340088194967515_WP , & ! Ocoeffs(gHO2,gC12H25O2)
       0.00157935233346629_WP , & ! Ocoeffs(gCH2O,gC12H25O2)
       0.00225631008527802_WP , & ! Ocoeffs(gCO2,gC12H25O2)
       0.00293705751871459_WP , & ! Ocoeffs(gCH3,gC12H25O2)
       0.00355843628542278_WP , & ! Ocoeffs(gCO,gC12H25O2)
       0.00221888642164156_WP , & ! Ocoeffs(gC2H6,gC12H25O2)
       0.0029639371692608_WP , & ! Ocoeffs(gCH4,gC12H25O2)
       0.00210327057506087_WP , & ! Ocoeffs(gC2H4,gC12H25O2)
       0.00243792620028179_WP , & ! Ocoeffs(gC2H2,gC12H25O2)
       0.00215774831015193_WP , & ! Ocoeffs(gC3H6,gC12H25O2)
       0.00186534681407191_WP , & ! Ocoeffs(gC4H81,gC12H25O2)
       0.00166421181455306_WP , & ! Ocoeffs(gC5H10,gC12H25O2)
       0.0015689445514597_WP , & ! Ocoeffs(gC6H12,gC12H25O2)
       0.00149209757448193_WP , & ! Ocoeffs(gC7H14,gC12H25O2)
       0.00142878765385081_WP , & ! Ocoeffs(gC8H16,gC12H25O2)
       0.00137671420177066_WP , & ! Ocoeffs(gC9H18,gC12H25O2)
       0.0013339146296626_WP , & ! Ocoeffs(gC10H20,gC12H25O2)
       0.00124218818902626_WP , & ! Ocoeffs(gC12H25O2,gC12H25O2)
       0.00125396567777747_WP , & ! Ocoeffs(gNXC12H26,gC12H25O2)
       0.00121652956466567_WP , & ! Ocoeffs(gOC12H23OOH,gC12H25O2)
       0.00293705751871459_WP , & ! Ocoeffs(gCH2,gC12H25O2)
       0.00157935233346629_WP , & ! Ocoeffs(gHCO,gC12H25O2)
       0.00293705751871459_WP , & ! Ocoeffs(gCH2D,gC12H25O2)
       0.00172593990591097_WP , & ! Ocoeffs(gCH3O,gC12H25O2)
       0.00243792620028179_WP , & ! Ocoeffs(gC2H3,gC12H25O2)
       0.00168791453557875_WP , & ! Ocoeffs(gCH2CHO,gC12H25O2)
       0.00221888642164156_WP , & ! Ocoeffs(gC2H5,gC12H25O2)
       0.00215774831015193_WP , & ! Ocoeffs(gAXC3H5,gC12H25O2)
       0.00186534681407191_WP , & ! Ocoeffs(gC2H3CHO,gC12H25O2)
       0.00215774831015193_WP , & ! Ocoeffs(gNXC3H7,gC12H25O2)
       0.00186534681407191_WP , & ! Ocoeffs(gC4H7,gC12H25O2)
       0.00186534681407191_WP , & ! Ocoeffs(gPXC4H9,gC12H25O2)
       0.0016465491003578_WP , & ! Ocoeffs(gPXC5H11,gC12H25O2)
       0.00148402971065057_WP , & ! Ocoeffs(gPXC7H15,gC12H25O2)
       0.00125396567777747_WP , & ! Ocoeffs(gPXC12H25,gC12H25O2)
       0.00125396567777747_WP , & ! Ocoeffs(gS3XC12H25,gC12H25O2)
       0.00125396567777747_WP , & ! Ocoeffs(gSXC12H25,gC12H25O2)
       0.00124218818902626_WP , & ! Ocoeffs(gC12OOH,gC12H25O2)
       0.00119009334372922_WP , & ! Ocoeffs(gO2C12H24OOH,gC12H25O2)
       0.00360265636722638_WP , & ! Ocoeffs(gN2,gNXC12H26)
       0.00397783677356156_WP , & ! Ocoeffs(gO,gNXC12H26)
       0.0057716522120663_WP , & ! Ocoeffs(gH2,gNXC12H26)
       0.00295466297951851_WP , & ! Ocoeffs(gH,gNXC12H26)
       0.00397783677356156_WP , & ! Ocoeffs(gOH,gNXC12H26)
       0.00148710673075778_WP , & ! Ocoeffs(gH2O,gNXC12H26)
       0.00343312653971418_WP , & ! Ocoeffs(gH2O2,gNXC12H26)
       0.00343312653971418_WP , & ! Ocoeffs(gO2,gNXC12H26)
       0.00343312653971418_WP , & ! Ocoeffs(gHO2,gNXC12H26)
       0.00159432655758621_WP , & ! Ocoeffs(gCH2O,gNXC12H26)
       0.0022777027107138_WP , & ! Ocoeffs(gCO2,gNXC12H26)
       0.0029649044763607_WP , & ! Ocoeffs(gCH3,gNXC12H26)
       0.0035921746864909_WP , & ! Ocoeffs(gCO,gNXC12H26)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H6,gNXC12H26)
       0.00299203897942019_WP , & ! Ocoeffs(gCH4,gNXC12H26)
       0.00212321219562799_WP , & ! Ocoeffs(gC2H4,gNXC12H26)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H2,gNXC12H26)
       0.00217820644739333_WP , & ! Ocoeffs(gC3H6,gNXC12H26)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H81,gNXC12H26)
       0.00167999061208042_WP , & ! Ocoeffs(gC5H10,gNXC12H26)
       0.00158382009686363_WP , & ! Ocoeffs(gC6H12,gNXC12H26)
       0.00150624451498129_WP , & ! Ocoeffs(gC7H14,gNXC12H26)
       0.00144233433757374_WP , & ! Ocoeffs(gC8H16,gNXC12H26)
       0.00138976716441208_WP , & ! Ocoeffs(gC9H18,gNXC12H26)
       0.00134656179913716_WP , & ! Ocoeffs(gC10H20,gNXC12H26)
       0.00125396567777747_WP , & ! Ocoeffs(gC12H25O2,gNXC12H26)
       0.00126585483176789_WP , & ! Ocoeffs(gNXC12H26,gNXC12H26)
       0.00122806377775023_WP , & ! Ocoeffs(gOC12H23OOH,gNXC12H26)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2,gNXC12H26)
       0.00159432655758621_WP , & ! Ocoeffs(gHCO,gNXC12H26)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2D,gNXC12H26)
       0.00174230396250618_WP , & ! Ocoeffs(gCH3O,gNXC12H26)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H3,gNXC12H26)
       0.00170391806437688_WP , & ! Ocoeffs(gCH2CHO,gNXC12H26)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H5,gNXC12H26)
       0.00217820644739333_WP , & ! Ocoeffs(gAXC3H5,gNXC12H26)
       0.00188303262151551_WP , & ! Ocoeffs(gC2H3CHO,gNXC12H26)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC3H7,gNXC12H26)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H7,gNXC12H26)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC4H9,gNXC12H26)
       0.00166216043339018_WP , & ! Ocoeffs(gPXC5H11,gNXC12H26)
       0.00149810015776804_WP , & ! Ocoeffs(gPXC7H15,gNXC12H26)
       0.00126585483176789_WP , & ! Ocoeffs(gPXC12H25,gNXC12H26)
       0.00126585483176789_WP , & ! Ocoeffs(gS3XC12H25,gNXC12H26)
       0.00126585483176789_WP , & ! Ocoeffs(gSXC12H25,gNXC12H26)
       0.00125396567777747_WP , & ! Ocoeffs(gC12OOH,gNXC12H26)
       0.00120137690856462_WP , & ! Ocoeffs(gO2C12H24OOH,gNXC12H26)
       0.00349510202689907_WP , & ! Ocoeffs(gN2,gOC12H23OOH)
       0.0038590817310318_WP , & ! Ocoeffs(gO,gOC12H23OOH)
       0.005599344286194_WP , & ! Ocoeffs(gH2,gOC12H23OOH)
       0.00286645394838733_WP , & ! Ocoeffs(gH,gOC12H23OOH)
       0.0038590817310318_WP , & ! Ocoeffs(gOH,gOC12H23OOH)
       0.00144271038341864_WP , & ! Ocoeffs(gH2O,gOC12H23OOH)
       0.00333063337283927_WP , & ! Ocoeffs(gH2O2,gOC12H23OOH)
       0.00333063337283927_WP , & ! Ocoeffs(gO2,gOC12H23OOH)
       0.00333063337283927_WP , & ! Ocoeffs(gHO2,gOC12H23OOH)
       0.0015467292505748_WP , & ! Ocoeffs(gCH2O,gOC12H23OOH)
       0.00220970377116989_WP , & ! Ocoeffs(gCO2,gOC12H23OOH)
       0.00287638969377155_WP , & ! Ocoeffs(gCH3,gOC12H23OOH)
       0.00348493326811399_WP , & ! Ocoeffs(gCO,gOC12H23OOH)
       0.00217305313028146_WP , & ! Ocoeffs(gC2H6,gOC12H23OOH)
       0.00290271411857789_WP , & ! Ocoeffs(gCH4,gOC12H23OOH)
       0.00205982544324354_WP , & ! Ocoeffs(gC2H4,gOC12H23OOH)
       0.00238756842587651_WP , & ! Ocoeffs(gC2H2,gOC12H23OOH)
       0.00211317788689078_WP , & ! Ocoeffs(gC3H6,gOC12H23OOH)
       0.00182681623261306_WP , & ! Ocoeffs(gC4H81,gOC12H23OOH)
       0.00162983587523621_WP , & ! Ocoeffs(gC5H10,gOC12H23OOH)
       0.00153653645158873_WP , & ! Ocoeffs(gC6H12,gOC12H23OOH)
       0.00146127682484737_WP , & ! Ocoeffs(gC7H14,gOC12H23OOH)
       0.00139927463317884_WP , & ! Ocoeffs(gC8H16,gOC12H23OOH)
       0.00134827680970141_WP , & ! Ocoeffs(gC9H18,gOC12H23OOH)
       0.0013063613050424_WP , & ! Ocoeffs(gC10H20,gOC12H23OOH)
       0.00121652956466567_WP , & ! Ocoeffs(gC12H25O2,gOC12H23OOH)
       0.00122806377775023_WP , & ! Ocoeffs(gNXC12H26,gOC12H23OOH)
       0.00119140094454267_WP , & ! Ocoeffs(gOC12H23OOH,gOC12H23OOH)
       0.00287638969377155_WP , & ! Ocoeffs(gCH2,gOC12H23OOH)
       0.0015467292505748_WP , & ! Ocoeffs(gHCO,gOC12H23OOH)
       0.00287638969377155_WP , & ! Ocoeffs(gCH2D,gOC12H23OOH)
       0.00169028891187806_WP , & ! Ocoeffs(gCH3O,gOC12H23OOH)
       0.00238756842587651_WP , & ! Ocoeffs(gC2H3,gOC12H23OOH)
       0.00165304899314017_WP , & ! Ocoeffs(gCH2CHO,gOC12H23OOH)
       0.00217305313028146_WP , & ! Ocoeffs(gC2H5,gOC12H23OOH)
       0.00211317788689078_WP , & ! Ocoeffs(gAXC3H5,gOC12H23OOH)
       0.00182681623261306_WP , & ! Ocoeffs(gC2H3CHO,gOC12H23OOH)
       0.00211317788689078_WP , & ! Ocoeffs(gNXC3H7,gOC12H23OOH)
       0.00182681623261306_WP , & ! Ocoeffs(gC4H7,gOC12H23OOH)
       0.00182681623261306_WP , & ! Ocoeffs(gPXC4H9,gOC12H23OOH)
       0.0016125380018539_WP , & ! Ocoeffs(gPXC5H11,gOC12H23OOH)
       0.00145337561071472_WP , & ! Ocoeffs(gPXC7H15,gOC12H23OOH)
       0.00122806377775023_WP , & ! Ocoeffs(gPXC12H25,gOC12H23OOH)
       0.00122806377775023_WP , & ! Ocoeffs(gS3XC12H25,gOC12H23OOH)
       0.00122806377775023_WP , & ! Ocoeffs(gSXC12H25,gOC12H23OOH)
       0.00121652956466567_WP , & ! Ocoeffs(gC12OOH,gOC12H23OOH)
       0.00116551078986938_WP , & ! Ocoeffs(gO2C12H24OOH,gOC12H23OOH)
       0.00843819664144349_WP , & ! Ocoeffs(gN2,gCH2)
       0.00931694990624912_WP , & ! Ocoeffs(gO,gCH2)
       0.0135184517608969_WP , & ! Ocoeffs(gH2,gCH2)
       0.00692045665447833_WP , & ! Ocoeffs(gH,gCH2)
       0.00931694990624912_WP , & ! Ocoeffs(gOH,gCH2)
       0.00348312404566333_WP , & ! Ocoeffs(gH2O,gCH2)
       0.00804112129610901_WP , & ! Ocoeffs(gH2O2,gCH2)
       0.00804112129610901_WP , & ! Ocoeffs(gO2,gCH2)
       0.00804112129610901_WP , & ! Ocoeffs(gHO2,gCH2)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2O,gCH2)
       0.005334869997204_WP , & ! Ocoeffs(gCO2,gCH2)
       0.00694444444444444_WP , & ! Ocoeffs(gCH3,gCH2)
       0.00841364628910254_WP , & ! Ocoeffs(gCO,gCH2)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H6,gCH2)
       0.00700799929099234_WP , & ! Ocoeffs(gCH4,gCH2)
       0.00497301995860717_WP , & ! Ocoeffs(gC2H4,gCH2)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H2,gCH2)
       0.0051018283331073_WP , & ! Ocoeffs(gC3H6,gCH2)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H81,gCH2)
       0.00393489961170722_WP , & ! Ocoeffs(gC5H10,gCH2)
       0.0037096475654975_WP , & ! Ocoeffs(gC6H12,gCH2)
       0.00352794885549769_WP , & ! Ocoeffs(gC7H14,gCH2)
       0.0033782574640954_WP , & ! Ocoeffs(gC8H16,gCH2)
       0.00325513383008522_WP , & ! Ocoeffs(gC9H18,gCH2)
       0.00315393756516477_WP , & ! Ocoeffs(gC10H20,gCH2)
       0.00293705751871459_WP , & ! Ocoeffs(gC12H25O2,gCH2)
       0.0029649044763607_WP , & ! Ocoeffs(gNXC12H26,gCH2)
       0.00287638969377155_WP , & ! Ocoeffs(gOC12H23OOH,gCH2)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2,gCH2)
       0.00373425595790184_WP , & ! Ocoeffs(gHCO,gCH2)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2D,gCH2)
       0.00408085089061993_WP , & ! Ocoeffs(gCH3O,gCH2)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H3,gCH2)
       0.00399094285508813_WP , & ! Ocoeffs(gCH2CHO,gCH2)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H5,gCH2)
       0.0051018283331073_WP , & ! Ocoeffs(gAXC3H5,gCH2)
       0.00441046770020803_WP , & ! Ocoeffs(gC2H3CHO,gCH2)
       0.0051018283331073_WP , & ! Ocoeffs(gNXC3H7,gCH2)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H7,gCH2)
       0.00441046770020803_WP , & ! Ocoeffs(gPXC4H9,gCH2)
       0.00389313749547847_WP , & ! Ocoeffs(gPXC5H11,gCH2)
       0.0035088730179272_WP , & ! Ocoeffs(gPXC7H15,gCH2)
       0.0029649044763607_WP , & ! Ocoeffs(gPXC12H25,gCH2)
       0.0029649044763607_WP , & ! Ocoeffs(gS3XC12H25,gCH2)
       0.0029649044763607_WP , & ! Ocoeffs(gSXC12H25,gCH2)
       0.00293705751871459_WP , & ! Ocoeffs(gC12OOH,gCH2)
       0.00281388330210422_WP , & ! Ocoeffs(gO2C12H24OOH,gCH2)
       0.0045374955958451_WP , & ! Ocoeffs(gN2,gHCO)
       0.00501003010035126_WP , & ! Ocoeffs(gO,gHCO)
       0.00726931570028224_WP , & ! Ocoeffs(gH2,gHCO)
       0.00372135693504775_WP , & ! Ocoeffs(gH,gHCO)
       0.00501003010035126_WP , & ! Ocoeffs(gOH,gHCO)
       0.00187299024762664_WP , & ! Ocoeffs(gH2O,gHCO)
       0.00432397513558173_WP , & ! Ocoeffs(gH2O2,gHCO)
       0.00432397513558173_WP , & ! Ocoeffs(gO2,gHCO)
       0.00432397513558173_WP , & ! Ocoeffs(gHO2,gHCO)
       0.00200803212851406_WP , & ! Ocoeffs(gCH2O,gHCO)
       0.00286873489032348_WP , & ! Ocoeffs(gCO2,gHCO)
       0.00373425595790184_WP , & ! Ocoeffs(gCH3,gHCO)
       0.00452429406471742_WP , & ! Ocoeffs(gCO,gHCO)
       0.00282115341191848_WP , & ! Ocoeffs(gC2H6,gHCO)
       0.00376843148717184_WP , & ! Ocoeffs(gCH4,gHCO)
       0.00267415623492388_WP , & ! Ocoeffs(gC2H4,gHCO)
       0.00309964662943052_WP , & ! Ocoeffs(gC2H2,gHCO)
       0.00274342073027016_WP , & ! Ocoeffs(gC3H6,gHCO)
       0.0023716534012755_WP , & ! Ocoeffs(gC4H81,gHCO)
       0.00211592481390192_WP , & ! Ocoeffs(gC5H10,gHCO)
       0.00199479938733722_WP , & ! Ocoeffs(gC6H12,gHCO)
       0.00189709402072539_WP , & ! Ocoeffs(gC7H14,gHCO)
       0.00181660004101795_WP , & ! Ocoeffs(gC8H16,gHCO)
       0.00175039241742195_WP , & ! Ocoeffs(gC9H18,gHCO)
       0.00169597586067364_WP , & ! Ocoeffs(gC10H20,gHCO)
       0.00157935233346629_WP , & ! Ocoeffs(gC12H25O2,gHCO)
       0.00159432655758621_WP , & ! Ocoeffs(gNXC12H26,gHCO)
       0.0015467292505748_WP , & ! Ocoeffs(gOC12H23OOH,gHCO)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2,gHCO)
       0.00200803212851406_WP , & ! Ocoeffs(gHCO,gHCO)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2D,gHCO)
       0.00219440761223134_WP , & ! Ocoeffs(gCH3O,gHCO)
       0.00309964662943052_WP , & ! Ocoeffs(gC2H3,gHCO)
       0.00214606110733324_WP , & ! Ocoeffs(gCH2CHO,gHCO)
       0.00282115341191848_WP , & ! Ocoeffs(gC2H5,gHCO)
       0.00274342073027016_WP , & ! Ocoeffs(gAXC3H5,gHCO)
       0.0023716534012755_WP , & ! Ocoeffs(gC2H3CHO,gHCO)
       0.00274342073027016_WP , & ! Ocoeffs(gNXC3H7,gHCO)
       0.0023716534012755_WP , & ! Ocoeffs(gC4H7,gHCO)
       0.0023716534012755_WP , & ! Ocoeffs(gPXC4H9,gHCO)
       0.0020934679517887_WP , & ! Ocoeffs(gPXC5H11,gHCO)
       0.00188683631607105_WP , & ! Ocoeffs(gPXC7H15,gHCO)
       0.00159432655758621_WP , & ! Ocoeffs(gPXC12H25,gHCO)
       0.00159432655758621_WP , & ! Ocoeffs(gS3XC12H25,gHCO)
       0.00159432655758621_WP , & ! Ocoeffs(gSXC12H25,gHCO)
       0.00157935233346629_WP , & ! Ocoeffs(gC12OOH,gHCO)
       0.00151311750994414_WP , & ! Ocoeffs(gO2C12H24OOH,gHCO)
       0.00843819664144349_WP , & ! Ocoeffs(gN2,gCH2D)
       0.00931694990624912_WP , & ! Ocoeffs(gO,gCH2D)
       0.0135184517608969_WP , & ! Ocoeffs(gH2,gCH2D)
       0.00692045665447833_WP , & ! Ocoeffs(gH,gCH2D)
       0.00931694990624912_WP , & ! Ocoeffs(gOH,gCH2D)
       0.00348312404566333_WP , & ! Ocoeffs(gH2O,gCH2D)
       0.00804112129610901_WP , & ! Ocoeffs(gH2O2,gCH2D)
       0.00804112129610901_WP , & ! Ocoeffs(gO2,gCH2D)
       0.00804112129610901_WP , & ! Ocoeffs(gHO2,gCH2D)
       0.00373425595790184_WP , & ! Ocoeffs(gCH2O,gCH2D)
       0.005334869997204_WP , & ! Ocoeffs(gCO2,gCH2D)
       0.00694444444444444_WP , & ! Ocoeffs(gCH3,gCH2D)
       0.00841364628910254_WP , & ! Ocoeffs(gCO,gCH2D)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H6,gCH2D)
       0.00700799929099234_WP , & ! Ocoeffs(gCH4,gCH2D)
       0.00497301995860717_WP , & ! Ocoeffs(gC2H4,gCH2D)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H2,gCH2D)
       0.0051018283331073_WP , & ! Ocoeffs(gC3H6,gCH2D)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H81,gCH2D)
       0.00393489961170722_WP , & ! Ocoeffs(gC5H10,gCH2D)
       0.0037096475654975_WP , & ! Ocoeffs(gC6H12,gCH2D)
       0.00352794885549769_WP , & ! Ocoeffs(gC7H14,gCH2D)
       0.0033782574640954_WP , & ! Ocoeffs(gC8H16,gCH2D)
       0.00325513383008522_WP , & ! Ocoeffs(gC9H18,gCH2D)
       0.00315393756516477_WP , & ! Ocoeffs(gC10H20,gCH2D)
       0.00293705751871459_WP , & ! Ocoeffs(gC12H25O2,gCH2D)
       0.0029649044763607_WP , & ! Ocoeffs(gNXC12H26,gCH2D)
       0.00287638969377155_WP , & ! Ocoeffs(gOC12H23OOH,gCH2D)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2,gCH2D)
       0.00373425595790184_WP , & ! Ocoeffs(gHCO,gCH2D)
       0.00694444444444444_WP , & ! Ocoeffs(gCH2D,gCH2D)
       0.00408085089061993_WP , & ! Ocoeffs(gCH3O,gCH2D)
       0.00576428719888396_WP , & ! Ocoeffs(gC2H3,gCH2D)
       0.00399094285508813_WP , & ! Ocoeffs(gCH2CHO,gCH2D)
       0.00524638465043263_WP , & ! Ocoeffs(gC2H5,gCH2D)
       0.0051018283331073_WP , & ! Ocoeffs(gAXC3H5,gCH2D)
       0.00441046770020803_WP , & ! Ocoeffs(gC2H3CHO,gCH2D)
       0.0051018283331073_WP , & ! Ocoeffs(gNXC3H7,gCH2D)
       0.00441046770020803_WP , & ! Ocoeffs(gC4H7,gCH2D)
       0.00441046770020803_WP , & ! Ocoeffs(gPXC4H9,gCH2D)
       0.00389313749547847_WP , & ! Ocoeffs(gPXC5H11,gCH2D)
       0.0035088730179272_WP , & ! Ocoeffs(gPXC7H15,gCH2D)
       0.0029649044763607_WP , & ! Ocoeffs(gPXC12H25,gCH2D)
       0.0029649044763607_WP , & ! Ocoeffs(gS3XC12H25,gCH2D)
       0.0029649044763607_WP , & ! Ocoeffs(gSXC12H25,gCH2D)
       0.00293705751871459_WP , & ! Ocoeffs(gC12OOH,gCH2D)
       0.00281388330210422_WP , & ! Ocoeffs(gO2C12H24OOH,gCH2D)
       0.00495864320824235_WP , & ! Ocoeffs(gN2,gCH3O)
       0.00547503599848004_WP , & ! Ocoeffs(gO,gCH3O)
       0.00794401717078924_WP , & ! Ocoeffs(gH2,gCH3O)
       0.00406675464507713_WP , & ! Ocoeffs(gH,gCH3O)
       0.00547503599848004_WP , & ! Ocoeffs(gOH,gCH3O)
       0.00204683182039942_WP , & ! Ocoeffs(gH2O,gCH3O)
       0.00472530484840455_WP , & ! Ocoeffs(gH2O2,gCH3O)
       0.00472530484840455_WP , & ! Ocoeffs(gO2,gCH3O)
       0.00472530484840455_WP , & ! Ocoeffs(gHO2,gCH3O)
       0.00219440761223134_WP , & ! Ocoeffs(gCH2O,gCH3O)
       0.00313499649303813_WP , & ! Ocoeffs(gCO2,gCH3O)
       0.00408085089061993_WP , & ! Ocoeffs(gCH3,gCH3O)
       0.0049442163771233_WP , & ! Ocoeffs(gCO,gCH3O)
       0.00308299874014839_WP , & ! Ocoeffs(gC2H6,gCH3O)
       0.00411819842132783_WP , & ! Ocoeffs(gCH4,gCH3O)
       0.00292235802151_WP , & ! Ocoeffs(gC2H4,gCH3O)
       0.00338734030310707_WP , & ! Ocoeffs(gC2H2,gCH3O)
       0.00299805130036093_WP , & ! Ocoeffs(gC3H6,gCH3O)
       0.00259177839011199_WP , & ! Ocoeffs(gC4H81,gCH3O)
       0.0023123143562307_WP , & ! Ocoeffs(gC5H10,gCH3O)
       0.0021799466743027_WP , & ! Ocoeffs(gC6H12,gCH3O)
       0.00207317278497878_WP , & ! Ocoeffs(gC7H14,gCH3O)
       0.00198520775727799_WP , & ! Ocoeffs(gC8H16,gCH3O)
       0.00191285507370101_WP , & ! Ocoeffs(gC9H18,gCH3O)
       0.00185338784473377_WP , & ! Ocoeffs(gC10H20,gCH3O)
       0.00172593990591097_WP , & ! Ocoeffs(gC12H25O2,gCH3O)
       0.00174230396250618_WP , & ! Ocoeffs(gNXC12H26,gCH3O)
       0.00169028891187806_WP , & ! Ocoeffs(gOC12H23OOH,gCH3O)
       0.00408085089061993_WP , & ! Ocoeffs(gCH2,gCH3O)
       0.00219440761223134_WP , & ! Ocoeffs(gHCO,gCH3O)
       0.00408085089061993_WP , & ! Ocoeffs(gCH2D,gCH3O)
       0.00239808153477218_WP , & ! Ocoeffs(gCH3O,gCH3O)
       0.00338734030310707_WP , & ! Ocoeffs(gC2H3,gCH3O)
       0.00234524774946235_WP , & ! Ocoeffs(gCH2CHO,gCH3O)
       0.00308299874014839_WP , & ! Ocoeffs(gC2H5,gCH3O)
       0.00299805130036093_WP , & ! Ocoeffs(gAXC3H5,gCH3O)
       0.00259177839011199_WP , & ! Ocoeffs(gC2H3CHO,gCH3O)
       0.00299805130036093_WP , & ! Ocoeffs(gNXC3H7,gCH3O)
       0.00259177839011199_WP , & ! Ocoeffs(gC4H7,gCH3O)
       0.00259177839011199_WP , & ! Ocoeffs(gPXC4H9,gCH3O)
       0.002287773160665_WP , & ! Ocoeffs(gPXC5H11,gCH3O)
       0.00206196301156039_WP , & ! Ocoeffs(gPXC7H15,gCH3O)
       0.00174230396250618_WP , & ! Ocoeffs(gPXC12H25,gCH3O)
       0.00174230396250618_WP , & ! Ocoeffs(gS3XC12H25,gCH3O)
       0.00174230396250618_WP , & ! Ocoeffs(gSXC12H25,gCH3O)
       0.00172593990591097_WP , & ! Ocoeffs(gC12OOH,gCH3O)
       0.00165355749784693_WP , & ! Ocoeffs(gO2C12H24OOH,gCH3O)
       0.00700418719899912_WP , & ! Ocoeffs(gN2,gC2H3)
       0.00773360281112183_WP , & ! Ocoeffs(gO,gC2H3)
       0.0112210903345058_WP , & ! Ocoeffs(gH2,gC2H3)
       0.00574437595735306_WP , & ! Ocoeffs(gH,gC2H3)
       0.00773360281112183_WP , & ! Ocoeffs(gOH,gC2H3)
       0.00289119273819005_WP , & ! Ocoeffs(gH2O,gC2H3)
       0.00667459188746414_WP , & ! Ocoeffs(gH2O2,gC2H3)
       0.00667459188746414_WP , & ! Ocoeffs(gO2,gC2H3)
       0.00667459188746414_WP , & ! Ocoeffs(gHO2,gC2H3)
       0.00309964662943052_WP , & ! Ocoeffs(gCH2O,gC2H3)
       0.00442824808789341_WP , & ! Ocoeffs(gCO2,gC2H3)
       0.00576428719888396_WP , & ! Ocoeffs(gCH3,gC2H3)
       0.00698380899843042_WP , & ! Ocoeffs(gCO,gC2H3)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H6,gC2H3)
       0.00581704136681112_WP , & ! Ocoeffs(gCH4,gC2H3)
       0.0041278918013559_WP , & ! Ocoeffs(gC2H4,gC2H3)
       0.00478468899521531_WP , & ! Ocoeffs(gC2H2,gC2H3)
       0.00423481014020648_WP , & ! Ocoeffs(gC3H6,gC2H3)
       0.00366094116077764_WP , & ! Ocoeffs(gC4H81,gC2H3)
       0.00326619237033466_WP , & ! Ocoeffs(gC5H10,gC2H3)
       0.00307922025228023_WP , & ! Ocoeffs(gC6H12,gC2H3)
       0.00292839990135302_WP , & ! Ocoeffs(gC7H14,gC2H3)
       0.00280414746069398_WP , & ! Ocoeffs(gC8H16,gC2H3)
       0.00270194778250766_WP , & ! Ocoeffs(gC9H18,gC2H3)
       0.00261794907834603_WP , & ! Ocoeffs(gC10H20,gC2H3)
       0.00243792620028179_WP , & ! Ocoeffs(gC12H25O2,gC2H3)
       0.00246104077233596_WP , & ! Ocoeffs(gNXC12H26,gC2H3)
       0.00238756842587651_WP , & ! Ocoeffs(gOC12H23OOH,gC2H3)
       0.00576428719888396_WP , & ! Ocoeffs(gCH2,gC2H3)
       0.00309964662943052_WP , & ! Ocoeffs(gHCO,gC2H3)
       0.00576428719888396_WP , & ! Ocoeffs(gCH2D,gC2H3)
       0.00338734030310707_WP , & ! Ocoeffs(gCH3O,gC2H3)
       0.00478468899521531_WP , & ! Ocoeffs(gC2H3,gC2H3)
       0.00331271147679291_WP , & ! Ocoeffs(gCH2CHO,gC2H3)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H5,gC2H3)
       0.00423481014020648_WP , & ! Ocoeffs(gAXC3H5,gC2H3)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3CHO,gC2H3)
       0.00423481014020648_WP , & ! Ocoeffs(gNXC3H7,gC2H3)
       0.00366094116077764_WP , & ! Ocoeffs(gC4H7,gC2H3)
       0.00366094116077764_WP , & ! Ocoeffs(gPXC4H9,gC2H3)
       0.00323152741853016_WP , & ! Ocoeffs(gPXC5H11,gC2H3)
       0.00291256586204358_WP , & ! Ocoeffs(gPXC7H15,gC2H3)
       0.00246104077233596_WP , & ! Ocoeffs(gPXC12H25,gC2H3)
       0.00246104077233596_WP , & ! Ocoeffs(gS3XC12H25,gC2H3)
       0.00246104077233596_WP , & ! Ocoeffs(gSXC12H25,gC2H3)
       0.00243792620028179_WP , & ! Ocoeffs(gC12OOH,gC2H3)
       0.00233568453563607_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H3)
       0.00484939592582365_WP , & ! Ocoeffs(gN2,gCH2CHO)
       0.00535441171097649_WP , & ! Ocoeffs(gO,gCH2CHO)
       0.00776899705924871_WP , & ! Ocoeffs(gH2,gCH2CHO)
       0.00397715717363579_WP , & ! Ocoeffs(gH,gCH2CHO)
       0.00535441171097649_WP , & ! Ocoeffs(gOH,gCH2CHO)
       0.0020017366593733_WP , & ! Ocoeffs(gH2O,gCH2CHO)
       0.00462119840403887_WP , & ! Ocoeffs(gH2O2,gCH2CHO)
       0.00462119840403887_WP , & ! Ocoeffs(gO2,gCH2CHO)
       0.00462119840403887_WP , & ! Ocoeffs(gHO2,gCH2CHO)
       0.00214606110733324_WP , & ! Ocoeffs(gCH2O,gCH2CHO)
       0.00306592722693581_WP , & ! Ocoeffs(gCO2,gCH2CHO)
       0.00399094285508813_WP , & ! Ocoeffs(gCH3,gCH2CHO)
       0.00483528694215349_WP , & ! Ocoeffs(gCO,gCH2CHO)
       0.00301507507233909_WP , & ! Ocoeffs(gC2H6,gCH2CHO)
       0.00402746755663419_WP , & ! Ocoeffs(gCH4,gCH2CHO)
       0.00285797353997001_WP , & ! Ocoeffs(gC2H4,gCH2CHO)
       0.00331271147679291_WP , & ! Ocoeffs(gC2H2,gCH2CHO)
       0.00293199916808171_WP , & ! Ocoeffs(gC3H6,gCH2CHO)
       0.00253467713602688_WP , & ! Ocoeffs(gC4H81,gCH2CHO)
       0.00226137016667981_WP , & ! Ocoeffs(gC5H10,gCH2CHO)
       0.0021319187682841_WP , & ! Ocoeffs(gC6H12,gCH2CHO)
       0.00202749728802694_WP , & ! Ocoeffs(gC7H14,gCH2CHO)
       0.00194147027841308_WP , & ! Ocoeffs(gC8H16,gCH2CHO)
       0.00187071164662094_WP , & ! Ocoeffs(gC9H18,gCH2CHO)
       0.00181255458111673_WP , & ! Ocoeffs(gC10H20,gCH2CHO)
       0.00168791453557875_WP , & ! Ocoeffs(gC12H25O2,gCH2CHO)
       0.00170391806437688_WP , & ! Ocoeffs(gNXC12H26,gCH2CHO)
       0.00165304899314017_WP , & ! Ocoeffs(gOC12H23OOH,gCH2CHO)
       0.00399094285508813_WP , & ! Ocoeffs(gCH2,gCH2CHO)
       0.00214606110733324_WP , & ! Ocoeffs(gHCO,gCH2CHO)
       0.00399094285508813_WP , & ! Ocoeffs(gCH2D,gCH2CHO)
       0.00234524774946235_WP , & ! Ocoeffs(gCH3O,gCH2CHO)
       0.00331271147679291_WP , & ! Ocoeffs(gC2H3,gCH2CHO)
       0.00229357798165138_WP , & ! Ocoeffs(gCH2CHO,gCH2CHO)
       0.00301507507233909_WP , & ! Ocoeffs(gC2H5,gCH2CHO)
       0.00293199916808171_WP , & ! Ocoeffs(gAXC3H5,gCH2CHO)
       0.00253467713602688_WP , & ! Ocoeffs(gC2H3CHO,gCH2CHO)
       0.00293199916808171_WP , & ! Ocoeffs(gNXC3H7,gCH2CHO)
       0.00253467713602688_WP , & ! Ocoeffs(gC4H7,gCH2CHO)
       0.00253467713602688_WP , & ! Ocoeffs(gPXC4H9,gCH2CHO)
       0.00223736965508959_WP , & ! Ocoeffs(gPXC5H11,gCH2CHO)
       0.00201653448484436_WP , & ! Ocoeffs(gPXC7H15,gCH2CHO)
       0.00170391806437688_WP , & ! Ocoeffs(gPXC12H25,gCH2CHO)
       0.00170391806437688_WP , & ! Ocoeffs(gS3XC12H25,gCH2CHO)
       0.00170391806437688_WP , & ! Ocoeffs(gSXC12H25,gCH2CHO)
       0.00168791453557875_WP , & ! Ocoeffs(gC12OOH,gCH2CHO)
       0.00161712683418019_WP , & ! Ocoeffs(gO2C12H24OOH,gCH2CHO)
       0.00637488364852818_WP , & ! Ocoeffs(gN2,gC2H5)
       0.00703876362868729_WP , & ! Ocoeffs(gO,gC2H5)
       0.0102129116855016_WP , & ! Ocoeffs(gH2,gC2H5)
       0.00522826236950968_WP , & ! Ocoeffs(gH,gC2H5)
       0.00703876362868729_WP , & ! Ocoeffs(gOH,gC2H5)
       0.0026314284281358_WP , & ! Ocoeffs(gH2O,gC2H5)
       0.00607490140898494_WP , & ! Ocoeffs(gH2O2,gC2H5)
       0.00607490140898494_WP , & ! Ocoeffs(gO2,gC2H5)
       0.00607490140898494_WP , & ! Ocoeffs(gHO2,gC2H5)
       0.00282115341191848_WP , & ! Ocoeffs(gCH2O,gC2H5)
       0.00403038432941538_WP , & ! Ocoeffs(gCO2,gC2H5)
       0.00524638465043263_WP , & ! Ocoeffs(gCH3,gC2H5)
       0.00635633636332564_WP , & ! Ocoeffs(gCO,gC2H5)
       0.00396353547364249_WP , & ! Ocoeffs(gC2H6,gC2H5)
       0.00529439902711271_WP , & ! Ocoeffs(gCH4,gC2H5)
       0.00375701408310697_WP , & ! Ocoeffs(gC2H4,gC2H5)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H2,gC2H5)
       0.00385432615525772_WP , & ! Ocoeffs(gC3H6,gC2H5)
       0.00333201744627844_WP , & ! Ocoeffs(gC4H81,gC2H5)
       0.00297273555703498_WP , & ! Ocoeffs(gC5H10,gC2H5)
       0.00280256227864428_WP , & ! Ocoeffs(gC6H12,gC2H5)
       0.0026652926481112_WP , & ! Ocoeffs(gC7H14,gC2H5)
       0.0025522038870969_WP , & ! Ocoeffs(gC8H16,gC2H5)
       0.00245918651922188_WP , & ! Ocoeffs(gC9H18,gC2H5)
       0.00238273482676367_WP , & ! Ocoeffs(gC10H20,gC2H5)
       0.00221888642164156_WP , & ! Ocoeffs(gC12H25O2,gC2H5)
       0.00223992422420799_WP , & ! Ocoeffs(gNXC12H26,gC2H5)
       0.00217305313028146_WP , & ! Ocoeffs(gOC12H23OOH,gC2H5)
       0.00524638465043263_WP , & ! Ocoeffs(gCH2,gC2H5)
       0.00282115341191848_WP , & ! Ocoeffs(gHCO,gC2H5)
       0.00524638465043263_WP , & ! Ocoeffs(gCH2D,gC2H5)
       0.00308299874014839_WP , & ! Ocoeffs(gCH3O,gC2H5)
       0.00435480017485105_WP , & ! Ocoeffs(gC2H3,gC2H5)
       0.00301507507233909_WP , & ! Ocoeffs(gCH2CHO,gC2H5)
       0.00396353547364249_WP , & ! Ocoeffs(gC2H5,gC2H5)
       0.00385432615525772_WP , & ! Ocoeffs(gAXC3H5,gC2H5)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H3CHO,gC2H5)
       0.00385432615525772_WP , & ! Ocoeffs(gNXC3H7,gC2H5)
       0.00333201744627844_WP , & ! Ocoeffs(gC4H7,gC2H5)
       0.00333201744627844_WP , & ! Ocoeffs(gPXC4H9,gC2H5)
       0.00294118513895548_WP , & ! Ocoeffs(gPXC5H11,gC2H5)
       0.00265088124598615_WP , & ! Ocoeffs(gPXC7H15,gC2H5)
       0.00223992422420799_WP , & ! Ocoeffs(gPXC12H25,gC2H5)
       0.00223992422420799_WP , & ! Ocoeffs(gS3XC12H25,gC2H5)
       0.00223992422420799_WP , & ! Ocoeffs(gSXC12H25,gC2H5)
       0.00221888642164156_WP , & ! Ocoeffs(gC12OOH,gC2H5)
       0.00212583083965463_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H5)
       0.00619923322161321_WP , & ! Ocoeffs(gN2,gAXC3H5)
       0.00684482097741742_WP , & ! Ocoeffs(gO,gAXC3H5)
       0.00993151011074227_WP , & ! Ocoeffs(gH2,gAXC3H5)
       0.00508420538465163_WP , & ! Ocoeffs(gH,gAXC3H5)
       0.00684482097741742_WP , & ! Ocoeffs(gOH,gAXC3H5)
       0.00255892333592052_WP , & ! Ocoeffs(gH2O,gAXC3H5)
       0.00590751654601557_WP , & ! Ocoeffs(gH2O2,gAXC3H5)
       0.00590751654601557_WP , & ! Ocoeffs(gO2,gAXC3H5)
       0.00590751654601557_WP , & ! Ocoeffs(gHO2,gAXC3H5)
       0.00274342073027016_WP , & ! Ocoeffs(gCH2O,gAXC3H5)
       0.00391933309034584_WP , & ! Ocoeffs(gCO2,gAXC3H5)
       0.0051018283331073_WP , & ! Ocoeffs(gCH3,gAXC3H5)
       0.00618119697923804_WP , & ! Ocoeffs(gCO,gAXC3H5)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H6,gAXC3H5)
       0.00514851974513_WP , & ! Ocoeffs(gCH4,gAXC3H5)
       0.00365349515413394_WP , & ! Ocoeffs(gC2H4,gAXC3H5)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H2,gAXC3H5)
       0.00374812593703148_WP , & ! Ocoeffs(gC3H6,gAXC3H5)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H81,gAXC3H5)
       0.00289082625507948_WP , & ! Ocoeffs(gC5H10,gAXC3H5)
       0.00272534184799167_WP , & ! Ocoeffs(gC6H12,gAXC3H5)
       0.00259185447773735_WP , & ! Ocoeffs(gC7H14,gAXC3H5)
       0.00248188170914685_WP , & ! Ocoeffs(gC8H16,gAXC3H5)
       0.00239142729634342_WP , & ! Ocoeffs(gC9H18,gAXC3H5)
       0.00231708211643651_WP , & ! Ocoeffs(gC10H20,gAXC3H5)
       0.00215774831015193_WP , & ! Ocoeffs(gC12H25O2,gAXC3H5)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC12H26,gAXC3H5)
       0.00211317788689078_WP , & ! Ocoeffs(gOC12H23OOH,gAXC3H5)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2,gAXC3H5)
       0.00274342073027016_WP , & ! Ocoeffs(gHCO,gAXC3H5)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2D,gAXC3H5)
       0.00299805130036093_WP , & ! Ocoeffs(gCH3O,gAXC3H5)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H3,gAXC3H5)
       0.00293199916808171_WP , & ! Ocoeffs(gCH2CHO,gAXC3H5)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H5,gAXC3H5)
       0.00374812593703148_WP , & ! Ocoeffs(gAXC3H5,gAXC3H5)
       0.00324020866682533_WP , & ! Ocoeffs(gC2H3CHO,gAXC3H5)
       0.00374812593703148_WP , & ! Ocoeffs(gNXC3H7,gAXC3H5)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H7,gAXC3H5)
       0.00324020866682533_WP , & ! Ocoeffs(gPXC4H9,gAXC3H5)
       0.00286014516179248_WP , & ! Ocoeffs(gPXC5H11,gAXC3H5)
       0.00257784016033969_WP , & ! Ocoeffs(gPXC7H15,gAXC3H5)
       0.00217820644739333_WP , & ! Ocoeffs(gPXC12H25,gAXC3H5)
       0.00217820644739333_WP , & ! Ocoeffs(gS3XC12H25,gAXC3H5)
       0.00217820644739333_WP , & ! Ocoeffs(gSXC12H25,gAXC3H5)
       0.00215774831015193_WP , & ! Ocoeffs(gC12OOH,gAXC3H5)
       0.00206725673616953_WP , & ! Ocoeffs(gO2C12H24OOH,gAXC3H5)
       0.00535916069785301_WP , & ! Ocoeffs(gN2,gC2H3CHO)
       0.00591726335413939_WP , & ! Ocoeffs(gO,gC2H3CHO)
       0.00858566805814886_WP , & ! Ocoeffs(gH2,gC2H3CHO)
       0.00439523287851836_WP , & ! Ocoeffs(gH,gC2H3CHO)
       0.00591726335413939_WP , & ! Ocoeffs(gOH,gC2H3CHO)
       0.00221215767828711_WP , & ! Ocoeffs(gH2O,gC2H3CHO)
       0.00510697522799189_WP , & ! Ocoeffs(gH2O2,gC2H3CHO)
       0.00510697522799189_WP , & ! Ocoeffs(gO2,gC2H3CHO)
       0.00510697522799189_WP , & ! Ocoeffs(gHO2,gC2H3CHO)
       0.0023716534012755_WP , & ! Ocoeffs(gCH2O,gC2H3CHO)
       0.00338821514027671_WP , & ! Ocoeffs(gCO2,gC2H3CHO)
       0.00441046770020803_WP , & ! Ocoeffs(gCH3,gC2H3CHO)
       0.00534356858866491_WP , & ! Ocoeffs(gCO,gC2H3CHO)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H6,gC2H3CHO)
       0.00445083185030435_WP , & ! Ocoeffs(gCH4,gC2H3CHO)
       0.00315840152158946_WP , & ! Ocoeffs(gC2H4,gC2H3CHO)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H2,gC2H3CHO)
       0.00324020866682533_WP , & ! Ocoeffs(gC3H6,gC2H3CHO)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H81,gC2H3CHO)
       0.00249908366030339_WP , & ! Ocoeffs(gC5H10,gC2H3CHO)
       0.00235602443041662_WP , & ! Ocoeffs(gC6H12,gC2H3CHO)
       0.00224062624442282_WP , & ! Ocoeffs(gC7H14,gC2H3CHO)
       0.00214555614168664_WP , & ! Ocoeffs(gC8H16,gC2H3CHO)
       0.00206735941691212_WP , & ! Ocoeffs(gC9H18,gC2H3CHO)
       0.002003088925387_WP , & ! Ocoeffs(gC10H20,gC2H3CHO)
       0.00186534681407191_WP , & ! Ocoeffs(gC12H25O2,gC2H3CHO)
       0.00188303262151551_WP , & ! Ocoeffs(gNXC12H26,gC2H3CHO)
       0.00182681623261306_WP , & ! Ocoeffs(gOC12H23OOH,gC2H3CHO)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2,gC2H3CHO)
       0.0023716534012755_WP , & ! Ocoeffs(gHCO,gC2H3CHO)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2D,gC2H3CHO)
       0.00259177839011199_WP , & ! Ocoeffs(gCH3O,gC2H3CHO)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3,gC2H3CHO)
       0.00253467713602688_WP , & ! Ocoeffs(gCH2CHO,gC2H3CHO)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H5,gC2H3CHO)
       0.00324020866682533_WP , & ! Ocoeffs(gAXC3H5,gC2H3CHO)
       0.00280112044817927_WP , & ! Ocoeffs(gC2H3CHO,gC2H3CHO)
       0.00324020866682533_WP , & ! Ocoeffs(gNXC3H7,gC2H3CHO)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H7,gC2H3CHO)
       0.00280112044817927_WP , & ! Ocoeffs(gPXC4H9,gC2H3CHO)
       0.00247256023338382_WP , & ! Ocoeffs(gPXC5H11,gC2H3CHO)
       0.00222851103979671_WP , & ! Ocoeffs(gPXC7H15,gC2H3CHO)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC12H25,gC2H3CHO)
       0.00188303262151551_WP , & ! Ocoeffs(gS3XC12H25,gC2H3CHO)
       0.00188303262151551_WP , & ! Ocoeffs(gSXC12H25,gC2H3CHO)
       0.00186534681407191_WP , & ! Ocoeffs(gC12OOH,gC2H3CHO)
       0.0017871179639163_WP , & ! Ocoeffs(gO2C12H24OOH,gC2H3CHO)
       0.00619923322161321_WP , & ! Ocoeffs(gN2,gNXC3H7)
       0.00684482097741742_WP , & ! Ocoeffs(gO,gNXC3H7)
       0.00993151011074227_WP , & ! Ocoeffs(gH2,gNXC3H7)
       0.00508420538465163_WP , & ! Ocoeffs(gH,gNXC3H7)
       0.00684482097741742_WP , & ! Ocoeffs(gOH,gNXC3H7)
       0.00255892333592052_WP , & ! Ocoeffs(gH2O,gNXC3H7)
       0.00590751654601557_WP , & ! Ocoeffs(gH2O2,gNXC3H7)
       0.00590751654601557_WP , & ! Ocoeffs(gO2,gNXC3H7)
       0.00590751654601557_WP , & ! Ocoeffs(gHO2,gNXC3H7)
       0.00274342073027016_WP , & ! Ocoeffs(gCH2O,gNXC3H7)
       0.00391933309034584_WP , & ! Ocoeffs(gCO2,gNXC3H7)
       0.0051018283331073_WP , & ! Ocoeffs(gCH3,gNXC3H7)
       0.00618119697923804_WP , & ! Ocoeffs(gCO,gNXC3H7)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H6,gNXC3H7)
       0.00514851974513_WP , & ! Ocoeffs(gCH4,gNXC3H7)
       0.00365349515413394_WP , & ! Ocoeffs(gC2H4,gNXC3H7)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H2,gNXC3H7)
       0.00374812593703148_WP , & ! Ocoeffs(gC3H6,gNXC3H7)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H81,gNXC3H7)
       0.00289082625507948_WP , & ! Ocoeffs(gC5H10,gNXC3H7)
       0.00272534184799167_WP , & ! Ocoeffs(gC6H12,gNXC3H7)
       0.00259185447773735_WP , & ! Ocoeffs(gC7H14,gNXC3H7)
       0.00248188170914685_WP , & ! Ocoeffs(gC8H16,gNXC3H7)
       0.00239142729634342_WP , & ! Ocoeffs(gC9H18,gNXC3H7)
       0.00231708211643651_WP , & ! Ocoeffs(gC10H20,gNXC3H7)
       0.00215774831015193_WP , & ! Ocoeffs(gC12H25O2,gNXC3H7)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC12H26,gNXC3H7)
       0.00211317788689078_WP , & ! Ocoeffs(gOC12H23OOH,gNXC3H7)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2,gNXC3H7)
       0.00274342073027016_WP , & ! Ocoeffs(gHCO,gNXC3H7)
       0.0051018283331073_WP , & ! Ocoeffs(gCH2D,gNXC3H7)
       0.00299805130036093_WP , & ! Ocoeffs(gCH3O,gNXC3H7)
       0.00423481014020648_WP , & ! Ocoeffs(gC2H3,gNXC3H7)
       0.00293199916808171_WP , & ! Ocoeffs(gCH2CHO,gNXC3H7)
       0.00385432615525772_WP , & ! Ocoeffs(gC2H5,gNXC3H7)
       0.00374812593703148_WP , & ! Ocoeffs(gAXC3H5,gNXC3H7)
       0.00324020866682533_WP , & ! Ocoeffs(gC2H3CHO,gNXC3H7)
       0.00374812593703148_WP , & ! Ocoeffs(gNXC3H7,gNXC3H7)
       0.00324020866682533_WP , & ! Ocoeffs(gC4H7,gNXC3H7)
       0.00324020866682533_WP , & ! Ocoeffs(gPXC4H9,gNXC3H7)
       0.00286014516179248_WP , & ! Ocoeffs(gPXC5H11,gNXC3H7)
       0.00257784016033969_WP , & ! Ocoeffs(gPXC7H15,gNXC3H7)
       0.00217820644739333_WP , & ! Ocoeffs(gPXC12H25,gNXC3H7)
       0.00217820644739333_WP , & ! Ocoeffs(gS3XC12H25,gNXC3H7)
       0.00217820644739333_WP , & ! Ocoeffs(gSXC12H25,gNXC3H7)
       0.00215774831015193_WP , & ! Ocoeffs(gC12OOH,gNXC3H7)
       0.00206725673616953_WP , & ! Ocoeffs(gO2C12H24OOH,gNXC3H7)
       0.00535916069785301_WP , & ! Ocoeffs(gN2,gC4H7)
       0.00591726335413939_WP , & ! Ocoeffs(gO,gC4H7)
       0.00858566805814886_WP , & ! Ocoeffs(gH2,gC4H7)
       0.00439523287851836_WP , & ! Ocoeffs(gH,gC4H7)
       0.00591726335413939_WP , & ! Ocoeffs(gOH,gC4H7)
       0.00221215767828711_WP , & ! Ocoeffs(gH2O,gC4H7)
       0.00510697522799189_WP , & ! Ocoeffs(gH2O2,gC4H7)
       0.00510697522799189_WP , & ! Ocoeffs(gO2,gC4H7)
       0.00510697522799189_WP , & ! Ocoeffs(gHO2,gC4H7)
       0.0023716534012755_WP , & ! Ocoeffs(gCH2O,gC4H7)
       0.00338821514027671_WP , & ! Ocoeffs(gCO2,gC4H7)
       0.00441046770020803_WP , & ! Ocoeffs(gCH3,gC4H7)
       0.00534356858866491_WP , & ! Ocoeffs(gCO,gC4H7)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H6,gC4H7)
       0.00445083185030435_WP , & ! Ocoeffs(gCH4,gC4H7)
       0.00315840152158946_WP , & ! Ocoeffs(gC2H4,gC4H7)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H2,gC4H7)
       0.00324020866682533_WP , & ! Ocoeffs(gC3H6,gC4H7)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H81,gC4H7)
       0.00249908366030339_WP , & ! Ocoeffs(gC5H10,gC4H7)
       0.00235602443041662_WP , & ! Ocoeffs(gC6H12,gC4H7)
       0.00224062624442282_WP , & ! Ocoeffs(gC7H14,gC4H7)
       0.00214555614168664_WP , & ! Ocoeffs(gC8H16,gC4H7)
       0.00206735941691212_WP , & ! Ocoeffs(gC9H18,gC4H7)
       0.002003088925387_WP , & ! Ocoeffs(gC10H20,gC4H7)
       0.00186534681407191_WP , & ! Ocoeffs(gC12H25O2,gC4H7)
       0.00188303262151551_WP , & ! Ocoeffs(gNXC12H26,gC4H7)
       0.00182681623261306_WP , & ! Ocoeffs(gOC12H23OOH,gC4H7)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2,gC4H7)
       0.0023716534012755_WP , & ! Ocoeffs(gHCO,gC4H7)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2D,gC4H7)
       0.00259177839011199_WP , & ! Ocoeffs(gCH3O,gC4H7)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3,gC4H7)
       0.00253467713602688_WP , & ! Ocoeffs(gCH2CHO,gC4H7)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H5,gC4H7)
       0.00324020866682533_WP , & ! Ocoeffs(gAXC3H5,gC4H7)
       0.00280112044817927_WP , & ! Ocoeffs(gC2H3CHO,gC4H7)
       0.00324020866682533_WP , & ! Ocoeffs(gNXC3H7,gC4H7)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H7,gC4H7)
       0.00280112044817927_WP , & ! Ocoeffs(gPXC4H9,gC4H7)
       0.00247256023338382_WP , & ! Ocoeffs(gPXC5H11,gC4H7)
       0.00222851103979671_WP , & ! Ocoeffs(gPXC7H15,gC4H7)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC12H25,gC4H7)
       0.00188303262151551_WP , & ! Ocoeffs(gS3XC12H25,gC4H7)
       0.00188303262151551_WP , & ! Ocoeffs(gSXC12H25,gC4H7)
       0.00186534681407191_WP , & ! Ocoeffs(gC12OOH,gC4H7)
       0.0017871179639163_WP , & ! Ocoeffs(gO2C12H24OOH,gC4H7)
       0.00535916069785301_WP , & ! Ocoeffs(gN2,gPXC4H9)
       0.00591726335413939_WP , & ! Ocoeffs(gO,gPXC4H9)
       0.00858566805814886_WP , & ! Ocoeffs(gH2,gPXC4H9)
       0.00439523287851836_WP , & ! Ocoeffs(gH,gPXC4H9)
       0.00591726335413939_WP , & ! Ocoeffs(gOH,gPXC4H9)
       0.00221215767828711_WP , & ! Ocoeffs(gH2O,gPXC4H9)
       0.00510697522799189_WP , & ! Ocoeffs(gH2O2,gPXC4H9)
       0.00510697522799189_WP , & ! Ocoeffs(gO2,gPXC4H9)
       0.00510697522799189_WP , & ! Ocoeffs(gHO2,gPXC4H9)
       0.0023716534012755_WP , & ! Ocoeffs(gCH2O,gPXC4H9)
       0.00338821514027671_WP , & ! Ocoeffs(gCO2,gPXC4H9)
       0.00441046770020803_WP , & ! Ocoeffs(gCH3,gPXC4H9)
       0.00534356858866491_WP , & ! Ocoeffs(gCO,gPXC4H9)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H6,gPXC4H9)
       0.00445083185030435_WP , & ! Ocoeffs(gCH4,gPXC4H9)
       0.00315840152158946_WP , & ! Ocoeffs(gC2H4,gPXC4H9)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H2,gPXC4H9)
       0.00324020866682533_WP , & ! Ocoeffs(gC3H6,gPXC4H9)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H81,gPXC4H9)
       0.00249908366030339_WP , & ! Ocoeffs(gC5H10,gPXC4H9)
       0.00235602443041662_WP , & ! Ocoeffs(gC6H12,gPXC4H9)
       0.00224062624442282_WP , & ! Ocoeffs(gC7H14,gPXC4H9)
       0.00214555614168664_WP , & ! Ocoeffs(gC8H16,gPXC4H9)
       0.00206735941691212_WP , & ! Ocoeffs(gC9H18,gPXC4H9)
       0.002003088925387_WP , & ! Ocoeffs(gC10H20,gPXC4H9)
       0.00186534681407191_WP , & ! Ocoeffs(gC12H25O2,gPXC4H9)
       0.00188303262151551_WP , & ! Ocoeffs(gNXC12H26,gPXC4H9)
       0.00182681623261306_WP , & ! Ocoeffs(gOC12H23OOH,gPXC4H9)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2,gPXC4H9)
       0.0023716534012755_WP , & ! Ocoeffs(gHCO,gPXC4H9)
       0.00441046770020803_WP , & ! Ocoeffs(gCH2D,gPXC4H9)
       0.00259177839011199_WP , & ! Ocoeffs(gCH3O,gPXC4H9)
       0.00366094116077764_WP , & ! Ocoeffs(gC2H3,gPXC4H9)
       0.00253467713602688_WP , & ! Ocoeffs(gCH2CHO,gPXC4H9)
       0.00333201744627844_WP , & ! Ocoeffs(gC2H5,gPXC4H9)
       0.00324020866682533_WP , & ! Ocoeffs(gAXC3H5,gPXC4H9)
       0.00280112044817927_WP , & ! Ocoeffs(gC2H3CHO,gPXC4H9)
       0.00324020866682533_WP , & ! Ocoeffs(gNXC3H7,gPXC4H9)
       0.00280112044817927_WP , & ! Ocoeffs(gC4H7,gPXC4H9)
       0.00280112044817927_WP , & ! Ocoeffs(gPXC4H9,gPXC4H9)
       0.00247256023338382_WP , & ! Ocoeffs(gPXC5H11,gPXC4H9)
       0.00222851103979671_WP , & ! Ocoeffs(gPXC7H15,gPXC4H9)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC12H25,gPXC4H9)
       0.00188303262151551_WP , & ! Ocoeffs(gS3XC12H25,gPXC4H9)
       0.00188303262151551_WP , & ! Ocoeffs(gSXC12H25,gPXC4H9)
       0.00186534681407191_WP , & ! Ocoeffs(gC12OOH,gPXC4H9)
       0.0017871179639163_WP , & ! Ocoeffs(gO2C12H24OOH,gPXC4H9)
       0.00473055260241948_WP , & ! Ocoeffs(gN2,gPXC5H11)
       0.00522319205138588_WP , & ! Ocoeffs(gO,gPXC5H11)
       0.00757860356608769_WP , & ! Ocoeffs(gH2,gPXC5H11)
       0.00387968965738316_WP , & ! Ocoeffs(gH,gPXC5H11)
       0.00522319205138588_WP , & ! Ocoeffs(gOH,gPXC5H11)
       0.00195268043859474_WP , & ! Ocoeffs(gH2O,gPXC5H11)
       0.00450794747859442_WP , & ! Ocoeffs(gH2O2,gPXC5H11)
       0.00450794747859442_WP , & ! Ocoeffs(gO2,gPXC5H11)
       0.00450794747859442_WP , & ! Ocoeffs(gHO2,gPXC5H11)
       0.0020934679517887_WP , & ! Ocoeffs(gCH2O,gPXC5H11)
       0.00299079106842499_WP , & ! Ocoeffs(gCO2,gPXC5H11)
       0.00389313749547847_WP , & ! Ocoeffs(gCH3,gPXC5H11)
       0.00471678938521896_WP , & ! Ocoeffs(gCO,gPXC5H11)
       0.00294118513895548_WP , & ! Ocoeffs(gC2H6,gPXC5H11)
       0.00392876709235902_WP , & ! Ocoeffs(gCH4,gPXC5H11)
       0.00278793366719276_WP , & ! Ocoeffs(gC2H4,gPXC5H11)
       0.00323152741853016_WP , & ! Ocoeffs(gC2H2,gPXC5H11)
       0.00286014516179248_WP , & ! Ocoeffs(gC3H6,gPXC5H11)
       0.00247256023338382_WP , & ! Ocoeffs(gC4H81,gPXC5H11)
       0.00220595115157647_WP , & ! Ocoeffs(gC5H10,gPXC5H11)
       0.00207967219664382_WP , & ! Ocoeffs(gC6H12,gPXC5H11)
       0.00197780975589145_WP , & ! Ocoeffs(gC7H14,gPXC5H11)
       0.00189389099561037_WP , & ! Ocoeffs(gC8H16,gPXC5H11)
       0.00182486643360555_WP , & ! Ocoeffs(gC9H18,gPXC5H11)
       0.00176813461344111_WP , & ! Ocoeffs(gC10H20,gPXC5H11)
       0.0016465491003578_WP , & ! Ocoeffs(gC12H25O2,gPXC5H11)
       0.00166216043339018_WP , & ! Ocoeffs(gNXC12H26,gPXC5H11)
       0.0016125380018539_WP , & ! Ocoeffs(gOC12H23OOH,gPXC5H11)
       0.00389313749547847_WP , & ! Ocoeffs(gCH2,gPXC5H11)
       0.0020934679517887_WP , & ! Ocoeffs(gHCO,gPXC5H11)
       0.00389313749547847_WP , & ! Ocoeffs(gCH2D,gPXC5H11)
       0.002287773160665_WP , & ! Ocoeffs(gCH3O,gPXC5H11)
       0.00323152741853016_WP , & ! Ocoeffs(gC2H3,gPXC5H11)
       0.00223736965508959_WP , & ! Ocoeffs(gCH2CHO,gPXC5H11)
       0.00294118513895548_WP , & ! Ocoeffs(gC2H5,gPXC5H11)
       0.00286014516179248_WP , & ! Ocoeffs(gAXC3H5,gPXC5H11)
       0.00247256023338382_WP , & ! Ocoeffs(gC2H3CHO,gPXC5H11)
       0.00286014516179248_WP , & ! Ocoeffs(gNXC3H7,gPXC5H11)
       0.00247256023338382_WP , & ! Ocoeffs(gC4H7,gPXC5H11)
       0.00247256023338382_WP , & ! Ocoeffs(gPXC4H9,gPXC5H11)
       0.00218253881645285_WP , & ! Ocoeffs(gPXC5H11,gPXC5H11)
       0.00196711561626697_WP , & ! Ocoeffs(gPXC7H15,gPXC5H11)
       0.00166216043339018_WP , & ! Ocoeffs(gPXC12H25,gPXC5H11)
       0.00166216043339018_WP , & ! Ocoeffs(gS3XC12H25,gPXC5H11)
       0.00166216043339018_WP , & ! Ocoeffs(gSXC12H25,gPXC5H11)
       0.0016465491003578_WP , & ! Ocoeffs(gC12OOH,gPXC5H11)
       0.00157749618115047_WP , & ! Ocoeffs(gO2C12H24OOH,gPXC5H11)
       0.004263632714178_WP , & ! Ocoeffs(gN2,gPXC7H15)
       0.00470764715550003_WP , & ! Ocoeffs(gO,gPXC7H15)
       0.00683057241042645_WP , & ! Ocoeffs(gH2,gPXC7H15)
       0.00349675252223526_WP , & ! Ocoeffs(gH,gPXC7H15)
       0.00470764715550003_WP , & ! Ocoeffs(gOH,gPXC7H15)
       0.00175994495739669_WP , & ! Ocoeffs(gH2O,gPXC7H15)
       0.00406299939117072_WP , & ! Ocoeffs(gH2O2,gPXC7H15)
       0.00406299939117072_WP , & ! Ocoeffs(gO2,gPXC7H15)
       0.00406299939117072_WP , & ! Ocoeffs(gHO2,gPXC7H15)
       0.00188683631607105_WP , & ! Ocoeffs(gCH2O,gPXC7H15)
       0.00269559091977674_WP , & ! Ocoeffs(gCO2,gPXC7H15)
       0.0035088730179272_WP , & ! Ocoeffs(gCH3,gPXC7H15)
       0.00425122796825499_WP , & ! Ocoeffs(gCO,gPXC7H15)
       0.00265088124598615_WP , & ! Ocoeffs(gC2H6,gPXC7H15)
       0.0035409858655415_WP , & ! Ocoeffs(gCH4,gPXC7H15)
       0.0025127561592533_WP , & ! Ocoeffs(gC2H4,gPXC7H15)
       0.00291256586204358_WP , & ! Ocoeffs(gC2H2,gPXC7H15)
       0.00257784016033969_WP , & ! Ocoeffs(gC3H6,gPXC7H15)
       0.00222851103979671_WP , & ! Ocoeffs(gC4H81,gPXC7H15)
       0.00198821708291112_WP , & ! Ocoeffs(gC5H10,gPXC7H15)
       0.00187440224379746_WP , & ! Ocoeffs(gC6H12,gPXC7H15)
       0.00178259393486635_WP , & ! Ocoeffs(gC7H14,gPXC7H15)
       0.00170695821072608_WP , & ! Ocoeffs(gC8H16,gPXC7H15)
       0.00164474658232244_WP , & ! Ocoeffs(gC9H18,gPXC7H15)
       0.00159361436485925_WP , & ! Ocoeffs(gC10H20,gPXC7H15)
       0.00148402971065057_WP , & ! Ocoeffs(gC12H25O2,gPXC7H15)
       0.00149810015776804_WP , & ! Ocoeffs(gNXC12H26,gPXC7H15)
       0.00145337561071472_WP , & ! Ocoeffs(gOC12H23OOH,gPXC7H15)
       0.0035088730179272_WP , & ! Ocoeffs(gCH2,gPXC7H15)
       0.00188683631607105_WP , & ! Ocoeffs(gHCO,gPXC7H15)
       0.0035088730179272_WP , & ! Ocoeffs(gCH2D,gPXC7H15)
       0.00206196301156039_WP , & ! Ocoeffs(gCH3O,gPXC7H15)
       0.00291256586204358_WP , & ! Ocoeffs(gC2H3,gPXC7H15)
       0.00201653448484436_WP , & ! Ocoeffs(gCH2CHO,gPXC7H15)
       0.00265088124598615_WP , & ! Ocoeffs(gC2H5,gPXC7H15)
       0.00257784016033969_WP , & ! Ocoeffs(gAXC3H5,gPXC7H15)
       0.00222851103979671_WP , & ! Ocoeffs(gC2H3CHO,gPXC7H15)
       0.00257784016033969_WP , & ! Ocoeffs(gNXC3H7,gPXC7H15)
       0.00222851103979671_WP , & ! Ocoeffs(gC4H7,gPXC7H15)
       0.00222851103979671_WP , & ! Ocoeffs(gPXC4H9,gPXC7H15)
       0.00196711561626697_WP , & ! Ocoeffs(gPXC5H11,gPXC7H15)
       0.001772955339255_WP , & ! Ocoeffs(gPXC7H15,gPXC7H15)
       0.00149810015776804_WP , & ! Ocoeffs(gPXC12H25,gPXC7H15)
       0.00149810015776804_WP , & ! Ocoeffs(gS3XC12H25,gPXC7H15)
       0.00149810015776804_WP , & ! Ocoeffs(gSXC12H25,gPXC7H15)
       0.00148402971065057_WP , & ! Ocoeffs(gC12OOH,gPXC7H15)
       0.00142179252398631_WP , & ! Ocoeffs(gO2C12H24OOH,gPXC7H15)
       0.00360265636722638_WP , & ! Ocoeffs(gN2,gPXC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gO,gPXC12H25)
       0.0057716522120663_WP , & ! Ocoeffs(gH2,gPXC12H25)
       0.00295466297951851_WP , & ! Ocoeffs(gH,gPXC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gOH,gPXC12H25)
       0.00148710673075778_WP , & ! Ocoeffs(gH2O,gPXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gH2O2,gPXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gO2,gPXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gHO2,gPXC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gCH2O,gPXC12H25)
       0.0022777027107138_WP , & ! Ocoeffs(gCO2,gPXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH3,gPXC12H25)
       0.0035921746864909_WP , & ! Ocoeffs(gCO,gPXC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H6,gPXC12H25)
       0.00299203897942019_WP , & ! Ocoeffs(gCH4,gPXC12H25)
       0.00212321219562799_WP , & ! Ocoeffs(gC2H4,gPXC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H2,gPXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gC3H6,gPXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H81,gPXC12H25)
       0.00167999061208042_WP , & ! Ocoeffs(gC5H10,gPXC12H25)
       0.00158382009686363_WP , & ! Ocoeffs(gC6H12,gPXC12H25)
       0.00150624451498129_WP , & ! Ocoeffs(gC7H14,gPXC12H25)
       0.00144233433757374_WP , & ! Ocoeffs(gC8H16,gPXC12H25)
       0.00138976716441208_WP , & ! Ocoeffs(gC9H18,gPXC12H25)
       0.00134656179913716_WP , & ! Ocoeffs(gC10H20,gPXC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12H25O2,gPXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gNXC12H26,gPXC12H25)
       0.00122806377775023_WP , & ! Ocoeffs(gOC12H23OOH,gPXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2,gPXC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gHCO,gPXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2D,gPXC12H25)
       0.00174230396250618_WP , & ! Ocoeffs(gCH3O,gPXC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H3,gPXC12H25)
       0.00170391806437688_WP , & ! Ocoeffs(gCH2CHO,gPXC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H5,gPXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gAXC3H5,gPXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC2H3CHO,gPXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC3H7,gPXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H7,gPXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC4H9,gPXC12H25)
       0.00166216043339018_WP , & ! Ocoeffs(gPXC5H11,gPXC12H25)
       0.00149810015776804_WP , & ! Ocoeffs(gPXC7H15,gPXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gPXC12H25,gPXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gS3XC12H25,gPXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gSXC12H25,gPXC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12OOH,gPXC12H25)
       0.00120137690856462_WP , & ! Ocoeffs(gO2C12H24OOH,gPXC12H25)
       0.00360265636722638_WP , & ! Ocoeffs(gN2,gS3XC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gO,gS3XC12H25)
       0.0057716522120663_WP , & ! Ocoeffs(gH2,gS3XC12H25)
       0.00295466297951851_WP , & ! Ocoeffs(gH,gS3XC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gOH,gS3XC12H25)
       0.00148710673075778_WP , & ! Ocoeffs(gH2O,gS3XC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gH2O2,gS3XC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gO2,gS3XC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gHO2,gS3XC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gCH2O,gS3XC12H25)
       0.0022777027107138_WP , & ! Ocoeffs(gCO2,gS3XC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH3,gS3XC12H25)
       0.0035921746864909_WP , & ! Ocoeffs(gCO,gS3XC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H6,gS3XC12H25)
       0.00299203897942019_WP , & ! Ocoeffs(gCH4,gS3XC12H25)
       0.00212321219562799_WP , & ! Ocoeffs(gC2H4,gS3XC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H2,gS3XC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gC3H6,gS3XC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H81,gS3XC12H25)
       0.00167999061208042_WP , & ! Ocoeffs(gC5H10,gS3XC12H25)
       0.00158382009686363_WP , & ! Ocoeffs(gC6H12,gS3XC12H25)
       0.00150624451498129_WP , & ! Ocoeffs(gC7H14,gS3XC12H25)
       0.00144233433757374_WP , & ! Ocoeffs(gC8H16,gS3XC12H25)
       0.00138976716441208_WP , & ! Ocoeffs(gC9H18,gS3XC12H25)
       0.00134656179913716_WP , & ! Ocoeffs(gC10H20,gS3XC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12H25O2,gS3XC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gNXC12H26,gS3XC12H25)
       0.00122806377775023_WP , & ! Ocoeffs(gOC12H23OOH,gS3XC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2,gS3XC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gHCO,gS3XC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2D,gS3XC12H25)
       0.00174230396250618_WP , & ! Ocoeffs(gCH3O,gS3XC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H3,gS3XC12H25)
       0.00170391806437688_WP , & ! Ocoeffs(gCH2CHO,gS3XC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H5,gS3XC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gAXC3H5,gS3XC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC2H3CHO,gS3XC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC3H7,gS3XC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H7,gS3XC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC4H9,gS3XC12H25)
       0.00166216043339018_WP , & ! Ocoeffs(gPXC5H11,gS3XC12H25)
       0.00149810015776804_WP , & ! Ocoeffs(gPXC7H15,gS3XC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gPXC12H25,gS3XC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gS3XC12H25,gS3XC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gSXC12H25,gS3XC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12OOH,gS3XC12H25)
       0.00120137690856462_WP , & ! Ocoeffs(gO2C12H24OOH,gS3XC12H25)
       0.00360265636722638_WP , & ! Ocoeffs(gN2,gSXC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gO,gSXC12H25)
       0.0057716522120663_WP , & ! Ocoeffs(gH2,gSXC12H25)
       0.00295466297951851_WP , & ! Ocoeffs(gH,gSXC12H25)
       0.00397783677356156_WP , & ! Ocoeffs(gOH,gSXC12H25)
       0.00148710673075778_WP , & ! Ocoeffs(gH2O,gSXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gH2O2,gSXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gO2,gSXC12H25)
       0.00343312653971418_WP , & ! Ocoeffs(gHO2,gSXC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gCH2O,gSXC12H25)
       0.0022777027107138_WP , & ! Ocoeffs(gCO2,gSXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH3,gSXC12H25)
       0.0035921746864909_WP , & ! Ocoeffs(gCO,gSXC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H6,gSXC12H25)
       0.00299203897942019_WP , & ! Ocoeffs(gCH4,gSXC12H25)
       0.00212321219562799_WP , & ! Ocoeffs(gC2H4,gSXC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H2,gSXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gC3H6,gSXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H81,gSXC12H25)
       0.00167999061208042_WP , & ! Ocoeffs(gC5H10,gSXC12H25)
       0.00158382009686363_WP , & ! Ocoeffs(gC6H12,gSXC12H25)
       0.00150624451498129_WP , & ! Ocoeffs(gC7H14,gSXC12H25)
       0.00144233433757374_WP , & ! Ocoeffs(gC8H16,gSXC12H25)
       0.00138976716441208_WP , & ! Ocoeffs(gC9H18,gSXC12H25)
       0.00134656179913716_WP , & ! Ocoeffs(gC10H20,gSXC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12H25O2,gSXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gNXC12H26,gSXC12H25)
       0.00122806377775023_WP , & ! Ocoeffs(gOC12H23OOH,gSXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2,gSXC12H25)
       0.00159432655758621_WP , & ! Ocoeffs(gHCO,gSXC12H25)
       0.0029649044763607_WP , & ! Ocoeffs(gCH2D,gSXC12H25)
       0.00174230396250618_WP , & ! Ocoeffs(gCH3O,gSXC12H25)
       0.00246104077233596_WP , & ! Ocoeffs(gC2H3,gSXC12H25)
       0.00170391806437688_WP , & ! Ocoeffs(gCH2CHO,gSXC12H25)
       0.00223992422420799_WP , & ! Ocoeffs(gC2H5,gSXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gAXC3H5,gSXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC2H3CHO,gSXC12H25)
       0.00217820644739333_WP , & ! Ocoeffs(gNXC3H7,gSXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gC4H7,gSXC12H25)
       0.00188303262151551_WP , & ! Ocoeffs(gPXC4H9,gSXC12H25)
       0.00166216043339018_WP , & ! Ocoeffs(gPXC5H11,gSXC12H25)
       0.00149810015776804_WP , & ! Ocoeffs(gPXC7H15,gSXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gPXC12H25,gSXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gS3XC12H25,gSXC12H25)
       0.00126585483176789_WP , & ! Ocoeffs(gSXC12H25,gSXC12H25)
       0.00125396567777747_WP , & ! Ocoeffs(gC12OOH,gSXC12H25)
       0.00120137690856462_WP , & ! Ocoeffs(gO2C12H24OOH,gSXC12H25)
       0.00356881952018071_WP , & ! Ocoeffs(gN2,gC12OOH)
       0.00394047615940362_WP , & ! Ocoeffs(gO,gC12OOH)
       0.00571744373554407_WP , & ! Ocoeffs(gH2,gC12OOH)
       0.00292691221199626_WP , & ! Ocoeffs(gH,gC12OOH)
       0.00394047615940362_WP , & ! Ocoeffs(gOH,gC12OOH)
       0.00147313953603807_WP , & ! Ocoeffs(gH2O,gC12OOH)
       0.00340088194967515_WP , & ! Ocoeffs(gH2O2,gC12OOH)
       0.00340088194967515_WP , & ! Ocoeffs(gO2,gC12OOH)
       0.00340088194967515_WP , & ! Ocoeffs(gHO2,gC12OOH)
       0.00157935233346629_WP , & ! Ocoeffs(gCH2O,gC12OOH)
       0.00225631008527802_WP , & ! Ocoeffs(gCO2,gC12OOH)
       0.00293705751871459_WP , & ! Ocoeffs(gCH3,gC12OOH)
       0.00355843628542278_WP , & ! Ocoeffs(gCO,gC12OOH)
       0.00221888642164156_WP , & ! Ocoeffs(gC2H6,gC12OOH)
       0.0029639371692608_WP , & ! Ocoeffs(gCH4,gC12OOH)
       0.00210327057506087_WP , & ! Ocoeffs(gC2H4,gC12OOH)
       0.00243792620028179_WP , & ! Ocoeffs(gC2H2,gC12OOH)
       0.00215774831015193_WP , & ! Ocoeffs(gC3H6,gC12OOH)
       0.00186534681407191_WP , & ! Ocoeffs(gC4H81,gC12OOH)
       0.00166421181455306_WP , & ! Ocoeffs(gC5H10,gC12OOH)
       0.0015689445514597_WP , & ! Ocoeffs(gC6H12,gC12OOH)
       0.00149209757448193_WP , & ! Ocoeffs(gC7H14,gC12OOH)
       0.00142878765385081_WP , & ! Ocoeffs(gC8H16,gC12OOH)
       0.00137671420177066_WP , & ! Ocoeffs(gC9H18,gC12OOH)
       0.0013339146296626_WP , & ! Ocoeffs(gC10H20,gC12OOH)
       0.00124218818902626_WP , & ! Ocoeffs(gC12H25O2,gC12OOH)
       0.00125396567777747_WP , & ! Ocoeffs(gNXC12H26,gC12OOH)
       0.00121652956466567_WP , & ! Ocoeffs(gOC12H23OOH,gC12OOH)
       0.00293705751871459_WP , & ! Ocoeffs(gCH2,gC12OOH)
       0.00157935233346629_WP , & ! Ocoeffs(gHCO,gC12OOH)
       0.00293705751871459_WP , & ! Ocoeffs(gCH2D,gC12OOH)
       0.00172593990591097_WP , & ! Ocoeffs(gCH3O,gC12OOH)
       0.00243792620028179_WP , & ! Ocoeffs(gC2H3,gC12OOH)
       0.00168791453557875_WP , & ! Ocoeffs(gCH2CHO,gC12OOH)
       0.00221888642164156_WP , & ! Ocoeffs(gC2H5,gC12OOH)
       0.00215774831015193_WP , & ! Ocoeffs(gAXC3H5,gC12OOH)
       0.00186534681407191_WP , & ! Ocoeffs(gC2H3CHO,gC12OOH)
       0.00215774831015193_WP , & ! Ocoeffs(gNXC3H7,gC12OOH)
       0.00186534681407191_WP , & ! Ocoeffs(gC4H7,gC12OOH)
       0.00186534681407191_WP , & ! Ocoeffs(gPXC4H9,gC12OOH)
       0.0016465491003578_WP , & ! Ocoeffs(gPXC5H11,gC12OOH)
       0.00148402971065057_WP , & ! Ocoeffs(gPXC7H15,gC12OOH)
       0.00125396567777747_WP , & ! Ocoeffs(gPXC12H25,gC12OOH)
       0.00125396567777747_WP , & ! Ocoeffs(gS3XC12H25,gC12OOH)
       0.00125396567777747_WP , & ! Ocoeffs(gSXC12H25,gC12OOH)
       0.00124218818902626_WP , & ! Ocoeffs(gC12OOH,gC12OOH)
       0.00119009334372922_WP , & ! Ocoeffs(gO2C12H24OOH,gC12OOH)
       0.00341915049060909_WP , & ! Ocoeffs(gN2,gO2C12H24OOH)
       0.00377522060655397_WP , & ! Ocoeffs(gO,gO2C12H24OOH)
       0.00547766577796164_WP , & ! Ocoeffs(gH2,gO2C12H24OOH)
       0.00280416346890806_WP , & ! Ocoeffs(gH,gO2C12H24OOH)
       0.00377522060655397_WP , & ! Ocoeffs(gOH,gO2C12H24OOH)
       0.00141135906113996_WP , & ! Ocoeffs(gH2O,gO2C12H24OOH)
       0.00325825588012548_WP , & ! Ocoeffs(gH2O2,gO2C12H24OOH)
       0.00325825588012548_WP , & ! Ocoeffs(gO2,gO2C12H24OOH)
       0.00325825588012548_WP , & ! Ocoeffs(gHO2,gO2C12H24OOH)
       0.00151311750994414_WP , & ! Ocoeffs(gCH2O,gO2C12H24OOH)
       0.0021616850309802_WP , & ! Ocoeffs(gCO2,gO2C12H24OOH)
       0.00281388330210422_WP , & ! Ocoeffs(gCH3,gO2C12H24OOH)
       0.00340920270759122_WP , & ! Ocoeffs(gCO,gO2C12H24OOH)
       0.00212583083965463_WP , & ! Ocoeffs(gC2H6,gO2C12H24OOH)
       0.00283963567479575_WP , & ! Ocoeffs(gCH4,gO2C12H24OOH)
       0.00201506368644803_WP , & ! Ocoeffs(gC2H4,gO2C12H24OOH)
       0.00233568453563607_WP , & ! Ocoeffs(gC2H2,gO2C12H24OOH)
       0.00206725673616953_WP , & ! Ocoeffs(gC3H6,gO2C12H24OOH)
       0.0017871179639163_WP , & ! Ocoeffs(gC4H81,gO2C12H24OOH)
       0.00159441815704887_WP , & ! Ocoeffs(gC5H10,gO2C12H24OOH)
       0.00150314620913928_WP , & ! Ocoeffs(gC6H12,gO2C12H24OOH)
       0.00142952203802342_WP , & ! Ocoeffs(gC7H14,gO2C12H24OOH)
       0.00136886720665349_WP , & ! Ocoeffs(gC8H16,gO2C12H24OOH)
       0.00131897760920516_WP , & ! Ocoeffs(gC9H18,gO2C12H24OOH)
       0.00127797296407148_WP , & ! Ocoeffs(gC10H20,gO2C12H24OOH)
       0.00119009334372922_WP , & ! Ocoeffs(gC12H25O2,gO2C12H24OOH)
       0.00120137690856462_WP , & ! Ocoeffs(gNXC12H26,gO2C12H24OOH)
       0.00116551078986938_WP , & ! Ocoeffs(gOC12H23OOH,gO2C12H24OOH)
       0.00281388330210422_WP , & ! Ocoeffs(gCH2,gO2C12H24OOH)
       0.00151311750994414_WP , & ! Ocoeffs(gHCO,gO2C12H24OOH)
       0.00281388330210422_WP , & ! Ocoeffs(gCH2D,gO2C12H24OOH)
       0.00165355749784693_WP , & ! Ocoeffs(gCH3O,gO2C12H24OOH)
       0.00233568453563607_WP , & ! Ocoeffs(gC2H3,gO2C12H24OOH)
       0.00161712683418019_WP , & ! Ocoeffs(gCH2CHO,gO2C12H24OOH)
       0.00212583083965463_WP , & ! Ocoeffs(gC2H5,gO2C12H24OOH)
       0.00206725673616953_WP , & ! Ocoeffs(gAXC3H5,gO2C12H24OOH)
       0.0017871179639163_WP , & ! Ocoeffs(gC2H3CHO,gO2C12H24OOH)
       0.00206725673616953_WP , & ! Ocoeffs(gNXC3H7,gO2C12H24OOH)
       0.0017871179639163_WP , & ! Ocoeffs(gC4H7,gO2C12H24OOH)
       0.0017871179639163_WP , & ! Ocoeffs(gPXC4H9,gO2C12H24OOH)
       0.00157749618115047_WP , & ! Ocoeffs(gPXC5H11,gO2C12H24OOH)
       0.00142179252398631_WP , & ! Ocoeffs(gPXC7H15,gO2C12H24OOH)
       0.00120137690856462_WP , & ! Ocoeffs(gPXC12H25,gO2C12H24OOH)
       0.00120137690856462_WP , & ! Ocoeffs(gS3XC12H25,gO2C12H24OOH)
       0.00120137690856462_WP , & ! Ocoeffs(gSXC12H25,gO2C12H24OOH)
       0.00119009334372922_WP , & ! Ocoeffs(gC12OOH,gO2C12H24OOH)
       0.00114018325025198_WP  & ! Ocoeffs(gO2C12H24OOH,gO2C12H24OOH)
  /), (/npS,npS/) )


contains

  ! ================================================ !
  ! Compute omegamu needed for transport calculation !
  ! ================================================ !
  real(WP) function fcmech_omegamu(T_)
    implicit none
    real(WP), intent(in) :: T_
    real(WP), parameter, dimension(9) :: mArray = (/&
         3.3530622607_WP, 2.53272006_WP, 2.9024238575_WP, &
         0.11186138893_WP,0.8662326188_WP, 1.3913958626_WP, &
         3.158490576_WP, 0.18973411754_WP, 0.00018682962894_WP/)
    integer :: arrIndex
    real(WP) :: omegamu_Nr, omegamu_Dr
    omegamu_Dr = mArray(9)
    do arrIndex = 1,4 
        omegamu_Dr = mArray(9-arrIndex) + T_*omegamu_Dr
    end do
    omegamu_Nr = mArray(4)
    do arrIndex = 1,3 
        omegamu_Nr = mArray(4-arrIndex) + T_*omegamu_Nr
    end do
    fcmech_omegamu=omegamu_Nr/omegamu_Dr
  end function fcmech_omegamu

  ! =============================================== !
  ! Compute omegaD needed for transport calculation !
  ! =============================================== !
  real(WP) function fcmech_omegaD(T_)
    implicit none
    real(WP), intent(in) :: T_
    real(WP), parameter, dimension(9) :: mArray = (/ &
         6.8728271691_WP, 9.4122316321_WP, 7.7442359037_WP, &
         0.23424661229_WP,1.45337701568_WP, 5.2269794238_WP, &
         9.7108519575_WP, 0.46539437353_WP, 0.00041908394781_WP/)
    integer :: arrIndex
    real(WP) :: omegaD_Nr, omegaD_Dr
    omegaD_Dr = mArray(9)
    do arrIndex = 1,4
        omegaD_Dr = mArray(9-arrIndex) + T_*omegaD_Dr
    end do
    omegaD_Nr = mArray(4)
    do arrIndex = 1,3
        omegaD_Nr = mArray(4-arrIndex) + T_*omegaD_Nr
    end do
    fcmech_omegaD=omegaD_Nr/omegaD_Dr
  end function fcmech_omegaD


  ! ================================================ !
  ! Gets Lindemann rate constant
  ! for pressure dependent reaction rates
  ! ================================================ !
  subroutine fcmech_getlindratecoeff (Tloc,pressure,k0,kinf,fc,concin,lindratecoeff)
    implicit none
    
    real(WP) :: Tloc,pressure,k0,kinf,fc
    real(WP) :: ntmp,ccoeff,dcoeff,lgknull
    real(WP) :: f
    real(WP) :: conc, concin
    real(WP) :: lindratecoeff
    
    if (concin.gt.0.0_WP) then
       conc = concin
    else
       conc = pressure / ( R_cst * Tloc )
    end if
    ntmp = 0.75_WP - 1.27_WP * dlog10( fc )
    
    ccoeff = - 0.4_WP - 0.67_WP * dlog10( fc )
    dcoeff = 0.14_WP
    k0 = k0 * conc / max(kinf, 1.0e-60_WP)
    lgknull = dlog10(k0)
    f = (lgknull+ccoeff)/(ntmp-dcoeff*(lgknull+ccoeff))
    f = fc**(1.0_WP / ( f * f + 1.0_WP ))
    lindratecoeff = kinf * f * k0 / ( 1.0_WP + k0 )
 
     return
   end subroutine fcmech_getlindratecoeff
 
end module fcmech


! ================= !
! Export Molar mass !
! ================= !
subroutine fcmech_get_molarmass(myWsp)
  use fcmech
  implicit none

  real(WP), dimension(npS) :: myWsp

  myWsp = Wsp

  return
end subroutine fcmech_get_molarmass

! ==================== !
! Export species names !
! ==================== !
subroutine fcmech_get_speciesnames( names )
  use fcmech
  implicit none

  character(len=str_medium), dimension(npS) :: names

  names(gN2) = 'N2'
  names(gO) = 'O'
  names(gH2) = 'H2'
  names(gH) = 'H'
  names(gOH) = 'OH'
  names(gH2O) = 'H2O'
  names(gH2O2) = 'H2O2'
  names(gO2) = 'O2'
  names(gHO2) = 'HO2'
  names(gCH2O) = 'CH2O'
  names(gCO2) = 'CO2'
  names(gCH3) = 'CH3'
  names(gCO) = 'CO'
  names(gC2H6) = 'C2H6'
  names(gCH4) = 'CH4'
  names(gC2H4) = 'C2H4'
  names(gC2H2) = 'C2H2'
  names(gC3H6) = 'C3H6'
  names(gC4H81) = 'C4H81'
  names(gC5H10) = 'C5H10'
  names(gC6H12) = 'C6H12'
  names(gC7H14) = 'C7H14'
  names(gC8H16) = 'C8H16'
  names(gC9H18) = 'C9H18'
  names(gC10H20) = 'C10H20'
  names(gC12H25O2) = 'C12H25O2'
  names(gNXC12H26) = 'N-C12H26'
  names(gOC12H23OOH) = 'OC12H23OOH'
  names(gCH2) = 'CH2'
  names(gHCO) = 'HCO'
  names(gCH2D) = 'CH2D'
  names(gCH3O) = 'CH3O'
  names(gC2H3) = 'C2H3'
  names(gCH2CHO) = 'CH2CHO'
  names(gC2H5) = 'C2H5'
  names(gAXC3H5) = 'A-C3H5'
  names(gC2H3CHO) = 'C2H3CHO'
  names(gNXC3H7) = 'N-C3H7'
  names(gC4H7) = 'C4H7'
  names(gPXC4H9) = 'P-C4H9'
  names(gPXC5H11) = 'P-C5H11'
  names(gPXC7H15) = 'P-C7H15'
  names(gPXC12H25) = 'P-C12H25'
  names(gS3XC12H25) = 'S3-C12H25'
  names(gSXC12H25) = 'S-C12H25'
  names(gC12OOH) = 'C12OOH'
  names(gO2C12H24OOH) = 'O2C12H24OOH'

  return
end subroutine fcmech_get_speciesnames

! ======================== !
! Export Number of species !
! ======================== !
subroutine fcmech_get_nspecies( myns )
  use fcmech
  implicit none

  integer :: myns

  myns = npS

  return
end subroutine fcmech_get_nspecies

! ========================== !
! Export number of reactions !
! ========================== !
subroutine fcmech_get_nreactions( mynr )
  use fcmech
  implicit none

  integer ::  mynr

  mynr = npR

  return
end subroutine fcmech_get_nreactions


! ================ !
! Third bodies !
! ================ !
subroutine fcmech_thirdbodies ( c, M )
  use fcmech
  implicit none

  real(WP), dimension(npS) :: c
  real(WP), dimension(npS+1:npS+npTB) :: M
  real(WP) :: sumC

  ! Get Third body concentrations 
  sumC = sum(c)
  M(mM4) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM5) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM8) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (2_WP)*c(gC2H4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM6) = sumC + (1_WP)*c(gH2) + (-1_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (2.6_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.75_WP)*c(gCO) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM10) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM12) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM7) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM1) = sumC + (1_WP)*c(gH2) + (5.3_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (2.6_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.75_WP)*c(gCO) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM2) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (2.6_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.75_WP)*c(gCO) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM13) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM14) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM3) = sumC + (10.89_WP)*c(gH2O) + (-0.15_WP)*c(gO2) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1.18_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.0900000000000001_WP)*c(gCO) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM11) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (-1_WP)*c(gCH3O) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 
  M(mM9) = sumC + (1_WP)*c(gH2) + (5_WP)*c(gH2O) + (-1_WP)*c(gCH2) + (-1_WP)*c(gHCO) + (1_WP)*c(gCO2) + (-1_WP)*c(gCH2D) + (0.5_WP)*c(gCO) + (2_WP)*c(gC2H6) + (1_WP)*c(gCH4) + (2_WP)*c(gC2H4) + (-1_WP)*c(gCH3O) + (2_WP)*c(gC2H2) + (-1_WP)*c(gC2H3) + (-1_WP)*c(gCH2CHO) + (-1_WP)*c(gC2H5) + (-1_WP)*c(gAXC3H5) + (-1_WP)*c(gC2H3CHO) + (-1_WP)*c(gNXC3H7) + (-1_WP)*c(gC4H7) + (-1_WP)*c(gPXC4H9) + (-1_WP)*c(gPXC5H11) + (-1_WP)*c(gPXC7H15) + (-1_WP)*c(gPXC12H25) + (-1_WP)*c(gS3XC12H25) + (-1_WP)*c(gSXC12H25) + (-1_WP)*c(gC12OOH) + (-1_WP)*c(gO2C12H24OOH) 

  return
end subroutine fcmech_thirdbodies

! ======================= !
! Third body efficiencies !
! ======================= !
subroutine fcmech_thirdbody_efficiency
  use fcmech
  implicit none

  ! Get Third body efficiencies 
  Meff = 0.0_WP
  Meff(1:npS,mM4) = Meff(1:npS,mM4) + 1.0_WP
  Meff(gH2,mM4) = Meff(gH2,mM4) + (1_WP)
  Meff(gH2O,mM4) = Meff(gH2O,mM4) + (5_WP)
  Meff(gCH2,mM4) = Meff(gCH2,mM4) + (-1_WP)
  Meff(gHCO,mM4) = Meff(gHCO,mM4) + (-1_WP)
  Meff(gCO2,mM4) = Meff(gCO2,mM4) + (1_WP)
  Meff(gCH2D,mM4) = Meff(gCH2D,mM4) + (-1_WP)
  Meff(gCO,mM4) = Meff(gCO,mM4) + (0.5_WP)
  Meff(gC2H6,mM4) = Meff(gC2H6,mM4) + (2_WP)
  Meff(gCH4,mM4) = Meff(gCH4,mM4) + (1_WP)
  Meff(gCH3O,mM4) = Meff(gCH3O,mM4) + (-1_WP)
  Meff(gC2H3,mM4) = Meff(gC2H3,mM4) + (-1_WP)
  Meff(gCH2CHO,mM4) = Meff(gCH2CHO,mM4) + (-1_WP)
  Meff(gC2H5,mM4) = Meff(gC2H5,mM4) + (-1_WP)
  Meff(gAXC3H5,mM4) = Meff(gAXC3H5,mM4) + (-1_WP)
  Meff(gC2H3CHO,mM4) = Meff(gC2H3CHO,mM4) + (-1_WP)
  Meff(gNXC3H7,mM4) = Meff(gNXC3H7,mM4) + (-1_WP)
  Meff(gC4H7,mM4) = Meff(gC4H7,mM4) + (-1_WP)
  Meff(gPXC4H9,mM4) = Meff(gPXC4H9,mM4) + (-1_WP)
  Meff(gPXC5H11,mM4) = Meff(gPXC5H11,mM4) + (-1_WP)
  Meff(gPXC7H15,mM4) = Meff(gPXC7H15,mM4) + (-1_WP)
  Meff(gPXC12H25,mM4) = Meff(gPXC12H25,mM4) + (-1_WP)
  Meff(gS3XC12H25,mM4) = Meff(gS3XC12H25,mM4) + (-1_WP)
  Meff(gSXC12H25,mM4) = Meff(gSXC12H25,mM4) + (-1_WP)
  Meff(gC12OOH,mM4) = Meff(gC12OOH,mM4) + (-1_WP)
  Meff(gO2C12H24OOH,mM4) = Meff(gO2C12H24OOH,mM4) + (-1_WP)

  Meff(1:npS,mM5) = Meff(1:npS,mM5) + 1.0_WP
  Meff(gH2,mM5) = Meff(gH2,mM5) + (1_WP)
  Meff(gH2O,mM5) = Meff(gH2O,mM5) + (5_WP)
  Meff(gCH2,mM5) = Meff(gCH2,mM5) + (-1_WP)
  Meff(gHCO,mM5) = Meff(gHCO,mM5) + (-1_WP)
  Meff(gCO2,mM5) = Meff(gCO2,mM5) + (1_WP)
  Meff(gCH2D,mM5) = Meff(gCH2D,mM5) + (-1_WP)
  Meff(gCO,mM5) = Meff(gCO,mM5) + (0.5_WP)
  Meff(gC2H6,mM5) = Meff(gC2H6,mM5) + (2_WP)
  Meff(gCH4,mM5) = Meff(gCH4,mM5) + (1_WP)
  Meff(gCH3O,mM5) = Meff(gCH3O,mM5) + (-1_WP)
  Meff(gC2H3,mM5) = Meff(gC2H3,mM5) + (-1_WP)
  Meff(gCH2CHO,mM5) = Meff(gCH2CHO,mM5) + (-1_WP)
  Meff(gC2H5,mM5) = Meff(gC2H5,mM5) + (-1_WP)
  Meff(gAXC3H5,mM5) = Meff(gAXC3H5,mM5) + (-1_WP)
  Meff(gC2H3CHO,mM5) = Meff(gC2H3CHO,mM5) + (-1_WP)
  Meff(gNXC3H7,mM5) = Meff(gNXC3H7,mM5) + (-1_WP)
  Meff(gC4H7,mM5) = Meff(gC4H7,mM5) + (-1_WP)
  Meff(gPXC4H9,mM5) = Meff(gPXC4H9,mM5) + (-1_WP)
  Meff(gPXC5H11,mM5) = Meff(gPXC5H11,mM5) + (-1_WP)
  Meff(gPXC7H15,mM5) = Meff(gPXC7H15,mM5) + (-1_WP)
  Meff(gPXC12H25,mM5) = Meff(gPXC12H25,mM5) + (-1_WP)
  Meff(gS3XC12H25,mM5) = Meff(gS3XC12H25,mM5) + (-1_WP)
  Meff(gSXC12H25,mM5) = Meff(gSXC12H25,mM5) + (-1_WP)
  Meff(gC12OOH,mM5) = Meff(gC12OOH,mM5) + (-1_WP)
  Meff(gO2C12H24OOH,mM5) = Meff(gO2C12H24OOH,mM5) + (-1_WP)

  Meff(1:npS,mM8) = Meff(1:npS,mM8) + 1.0_WP
  Meff(gH2,mM8) = Meff(gH2,mM8) + (1_WP)
  Meff(gH2O,mM8) = Meff(gH2O,mM8) + (5_WP)
  Meff(gCH2,mM8) = Meff(gCH2,mM8) + (-1_WP)
  Meff(gHCO,mM8) = Meff(gHCO,mM8) + (-1_WP)
  Meff(gCO2,mM8) = Meff(gCO2,mM8) + (1_WP)
  Meff(gCH2D,mM8) = Meff(gCH2D,mM8) + (-1_WP)
  Meff(gCO,mM8) = Meff(gCO,mM8) + (0.5_WP)
  Meff(gC2H6,mM8) = Meff(gC2H6,mM8) + (2_WP)
  Meff(gCH4,mM8) = Meff(gCH4,mM8) + (1_WP)
  Meff(gC2H4,mM8) = Meff(gC2H4,mM8) + (2_WP)
  Meff(gCH3O,mM8) = Meff(gCH3O,mM8) + (-1_WP)
  Meff(gC2H3,mM8) = Meff(gC2H3,mM8) + (-1_WP)
  Meff(gCH2CHO,mM8) = Meff(gCH2CHO,mM8) + (-1_WP)
  Meff(gC2H5,mM8) = Meff(gC2H5,mM8) + (-1_WP)
  Meff(gAXC3H5,mM8) = Meff(gAXC3H5,mM8) + (-1_WP)
  Meff(gC2H3CHO,mM8) = Meff(gC2H3CHO,mM8) + (-1_WP)
  Meff(gNXC3H7,mM8) = Meff(gNXC3H7,mM8) + (-1_WP)
  Meff(gC4H7,mM8) = Meff(gC4H7,mM8) + (-1_WP)
  Meff(gPXC4H9,mM8) = Meff(gPXC4H9,mM8) + (-1_WP)
  Meff(gPXC5H11,mM8) = Meff(gPXC5H11,mM8) + (-1_WP)
  Meff(gPXC7H15,mM8) = Meff(gPXC7H15,mM8) + (-1_WP)
  Meff(gPXC12H25,mM8) = Meff(gPXC12H25,mM8) + (-1_WP)
  Meff(gS3XC12H25,mM8) = Meff(gS3XC12H25,mM8) + (-1_WP)
  Meff(gSXC12H25,mM8) = Meff(gSXC12H25,mM8) + (-1_WP)
  Meff(gC12OOH,mM8) = Meff(gC12OOH,mM8) + (-1_WP)
  Meff(gO2C12H24OOH,mM8) = Meff(gO2C12H24OOH,mM8) + (-1_WP)

  Meff(1:npS,mM6) = Meff(1:npS,mM6) + 1.0_WP
  Meff(gH2,mM6) = Meff(gH2,mM6) + (1_WP)
  Meff(gH2O,mM6) = Meff(gH2O,mM6) + (-1_WP)
  Meff(gCH2,mM6) = Meff(gCH2,mM6) + (-1_WP)
  Meff(gHCO,mM6) = Meff(gHCO,mM6) + (-1_WP)
  Meff(gCO2,mM6) = Meff(gCO2,mM6) + (2.6_WP)
  Meff(gCH2D,mM6) = Meff(gCH2D,mM6) + (-1_WP)
  Meff(gCO,mM6) = Meff(gCO,mM6) + (0.75_WP)
  Meff(gCH3O,mM6) = Meff(gCH3O,mM6) + (-1_WP)
  Meff(gC2H3,mM6) = Meff(gC2H3,mM6) + (-1_WP)
  Meff(gCH2CHO,mM6) = Meff(gCH2CHO,mM6) + (-1_WP)
  Meff(gC2H5,mM6) = Meff(gC2H5,mM6) + (-1_WP)
  Meff(gAXC3H5,mM6) = Meff(gAXC3H5,mM6) + (-1_WP)
  Meff(gC2H3CHO,mM6) = Meff(gC2H3CHO,mM6) + (-1_WP)
  Meff(gNXC3H7,mM6) = Meff(gNXC3H7,mM6) + (-1_WP)
  Meff(gC4H7,mM6) = Meff(gC4H7,mM6) + (-1_WP)
  Meff(gPXC4H9,mM6) = Meff(gPXC4H9,mM6) + (-1_WP)
  Meff(gPXC5H11,mM6) = Meff(gPXC5H11,mM6) + (-1_WP)
  Meff(gPXC7H15,mM6) = Meff(gPXC7H15,mM6) + (-1_WP)
  Meff(gPXC12H25,mM6) = Meff(gPXC12H25,mM6) + (-1_WP)
  Meff(gS3XC12H25,mM6) = Meff(gS3XC12H25,mM6) + (-1_WP)
  Meff(gSXC12H25,mM6) = Meff(gSXC12H25,mM6) + (-1_WP)
  Meff(gC12OOH,mM6) = Meff(gC12OOH,mM6) + (-1_WP)
  Meff(gO2C12H24OOH,mM6) = Meff(gO2C12H24OOH,mM6) + (-1_WP)

  Meff(1:npS,mM10) = Meff(1:npS,mM10) + 1.0_WP
  Meff(gH2,mM10) = Meff(gH2,mM10) + (1_WP)
  Meff(gH2O,mM10) = Meff(gH2O,mM10) + (5_WP)
  Meff(gCH2,mM10) = Meff(gCH2,mM10) + (-1_WP)
  Meff(gHCO,mM10) = Meff(gHCO,mM10) + (-1_WP)
  Meff(gCO2,mM10) = Meff(gCO2,mM10) + (1_WP)
  Meff(gCH2D,mM10) = Meff(gCH2D,mM10) + (-1_WP)
  Meff(gCO,mM10) = Meff(gCO,mM10) + (0.5_WP)
  Meff(gC2H6,mM10) = Meff(gC2H6,mM10) + (2_WP)
  Meff(gCH4,mM10) = Meff(gCH4,mM10) + (1_WP)
  Meff(gCH3O,mM10) = Meff(gCH3O,mM10) + (-1_WP)
  Meff(gC2H3,mM10) = Meff(gC2H3,mM10) + (-1_WP)
  Meff(gCH2CHO,mM10) = Meff(gCH2CHO,mM10) + (-1_WP)
  Meff(gC2H5,mM10) = Meff(gC2H5,mM10) + (-1_WP)
  Meff(gAXC3H5,mM10) = Meff(gAXC3H5,mM10) + (-1_WP)
  Meff(gC2H3CHO,mM10) = Meff(gC2H3CHO,mM10) + (-1_WP)
  Meff(gNXC3H7,mM10) = Meff(gNXC3H7,mM10) + (-1_WP)
  Meff(gC4H7,mM10) = Meff(gC4H7,mM10) + (-1_WP)
  Meff(gPXC4H9,mM10) = Meff(gPXC4H9,mM10) + (-1_WP)
  Meff(gPXC5H11,mM10) = Meff(gPXC5H11,mM10) + (-1_WP)
  Meff(gPXC7H15,mM10) = Meff(gPXC7H15,mM10) + (-1_WP)
  Meff(gPXC12H25,mM10) = Meff(gPXC12H25,mM10) + (-1_WP)
  Meff(gS3XC12H25,mM10) = Meff(gS3XC12H25,mM10) + (-1_WP)
  Meff(gSXC12H25,mM10) = Meff(gSXC12H25,mM10) + (-1_WP)
  Meff(gC12OOH,mM10) = Meff(gC12OOH,mM10) + (-1_WP)
  Meff(gO2C12H24OOH,mM10) = Meff(gO2C12H24OOH,mM10) + (-1_WP)

  Meff(1:npS,mM12) = Meff(1:npS,mM12) + 1.0_WP
  Meff(gH2,mM12) = Meff(gH2,mM12) + (1_WP)
  Meff(gH2O,mM12) = Meff(gH2O,mM12) + (5_WP)
  Meff(gCH2,mM12) = Meff(gCH2,mM12) + (-1_WP)
  Meff(gHCO,mM12) = Meff(gHCO,mM12) + (-1_WP)
  Meff(gCO2,mM12) = Meff(gCO2,mM12) + (1_WP)
  Meff(gCH2D,mM12) = Meff(gCH2D,mM12) + (-1_WP)
  Meff(gCO,mM12) = Meff(gCO,mM12) + (0.5_WP)
  Meff(gC2H6,mM12) = Meff(gC2H6,mM12) + (2_WP)
  Meff(gCH4,mM12) = Meff(gCH4,mM12) + (1_WP)
  Meff(gCH3O,mM12) = Meff(gCH3O,mM12) + (-1_WP)
  Meff(gC2H3,mM12) = Meff(gC2H3,mM12) + (-1_WP)
  Meff(gCH2CHO,mM12) = Meff(gCH2CHO,mM12) + (-1_WP)
  Meff(gC2H5,mM12) = Meff(gC2H5,mM12) + (-1_WP)
  Meff(gAXC3H5,mM12) = Meff(gAXC3H5,mM12) + (-1_WP)
  Meff(gC2H3CHO,mM12) = Meff(gC2H3CHO,mM12) + (-1_WP)
  Meff(gNXC3H7,mM12) = Meff(gNXC3H7,mM12) + (-1_WP)
  Meff(gC4H7,mM12) = Meff(gC4H7,mM12) + (-1_WP)
  Meff(gPXC4H9,mM12) = Meff(gPXC4H9,mM12) + (-1_WP)
  Meff(gPXC5H11,mM12) = Meff(gPXC5H11,mM12) + (-1_WP)
  Meff(gPXC7H15,mM12) = Meff(gPXC7H15,mM12) + (-1_WP)
  Meff(gPXC12H25,mM12) = Meff(gPXC12H25,mM12) + (-1_WP)
  Meff(gS3XC12H25,mM12) = Meff(gS3XC12H25,mM12) + (-1_WP)
  Meff(gSXC12H25,mM12) = Meff(gSXC12H25,mM12) + (-1_WP)
  Meff(gC12OOH,mM12) = Meff(gC12OOH,mM12) + (-1_WP)
  Meff(gO2C12H24OOH,mM12) = Meff(gO2C12H24OOH,mM12) + (-1_WP)

  Meff(1:npS,mM7) = Meff(1:npS,mM7) + 1.0_WP
  Meff(gH2,mM7) = Meff(gH2,mM7) + (1_WP)
  Meff(gH2O,mM7) = Meff(gH2O,mM7) + (5_WP)
  Meff(gCH2,mM7) = Meff(gCH2,mM7) + (-1_WP)
  Meff(gHCO,mM7) = Meff(gHCO,mM7) + (-1_WP)
  Meff(gCO2,mM7) = Meff(gCO2,mM7) + (1_WP)
  Meff(gCH2D,mM7) = Meff(gCH2D,mM7) + (-1_WP)
  Meff(gCO,mM7) = Meff(gCO,mM7) + (0.5_WP)
  Meff(gC2H6,mM7) = Meff(gC2H6,mM7) + (2_WP)
  Meff(gCH4,mM7) = Meff(gCH4,mM7) + (1_WP)
  Meff(gCH3O,mM7) = Meff(gCH3O,mM7) + (-1_WP)
  Meff(gC2H3,mM7) = Meff(gC2H3,mM7) + (-1_WP)
  Meff(gCH2CHO,mM7) = Meff(gCH2CHO,mM7) + (-1_WP)
  Meff(gC2H5,mM7) = Meff(gC2H5,mM7) + (-1_WP)
  Meff(gAXC3H5,mM7) = Meff(gAXC3H5,mM7) + (-1_WP)
  Meff(gC2H3CHO,mM7) = Meff(gC2H3CHO,mM7) + (-1_WP)
  Meff(gNXC3H7,mM7) = Meff(gNXC3H7,mM7) + (-1_WP)
  Meff(gC4H7,mM7) = Meff(gC4H7,mM7) + (-1_WP)
  Meff(gPXC4H9,mM7) = Meff(gPXC4H9,mM7) + (-1_WP)
  Meff(gPXC5H11,mM7) = Meff(gPXC5H11,mM7) + (-1_WP)
  Meff(gPXC7H15,mM7) = Meff(gPXC7H15,mM7) + (-1_WP)
  Meff(gPXC12H25,mM7) = Meff(gPXC12H25,mM7) + (-1_WP)
  Meff(gS3XC12H25,mM7) = Meff(gS3XC12H25,mM7) + (-1_WP)
  Meff(gSXC12H25,mM7) = Meff(gSXC12H25,mM7) + (-1_WP)
  Meff(gC12OOH,mM7) = Meff(gC12OOH,mM7) + (-1_WP)
  Meff(gO2C12H24OOH,mM7) = Meff(gO2C12H24OOH,mM7) + (-1_WP)

  Meff(1:npS,mM1) = Meff(1:npS,mM1) + 1.0_WP
  Meff(gH2,mM1) = Meff(gH2,mM1) + (1_WP)
  Meff(gH2O,mM1) = Meff(gH2O,mM1) + (5.3_WP)
  Meff(gCH2,mM1) = Meff(gCH2,mM1) + (-1_WP)
  Meff(gHCO,mM1) = Meff(gHCO,mM1) + (-1_WP)
  Meff(gCO2,mM1) = Meff(gCO2,mM1) + (2.6_WP)
  Meff(gCH2D,mM1) = Meff(gCH2D,mM1) + (-1_WP)
  Meff(gCO,mM1) = Meff(gCO,mM1) + (0.75_WP)
  Meff(gCH3O,mM1) = Meff(gCH3O,mM1) + (-1_WP)
  Meff(gC2H3,mM1) = Meff(gC2H3,mM1) + (-1_WP)
  Meff(gCH2CHO,mM1) = Meff(gCH2CHO,mM1) + (-1_WP)
  Meff(gC2H5,mM1) = Meff(gC2H5,mM1) + (-1_WP)
  Meff(gAXC3H5,mM1) = Meff(gAXC3H5,mM1) + (-1_WP)
  Meff(gC2H3CHO,mM1) = Meff(gC2H3CHO,mM1) + (-1_WP)
  Meff(gNXC3H7,mM1) = Meff(gNXC3H7,mM1) + (-1_WP)
  Meff(gC4H7,mM1) = Meff(gC4H7,mM1) + (-1_WP)
  Meff(gPXC4H9,mM1) = Meff(gPXC4H9,mM1) + (-1_WP)
  Meff(gPXC5H11,mM1) = Meff(gPXC5H11,mM1) + (-1_WP)
  Meff(gPXC7H15,mM1) = Meff(gPXC7H15,mM1) + (-1_WP)
  Meff(gPXC12H25,mM1) = Meff(gPXC12H25,mM1) + (-1_WP)
  Meff(gS3XC12H25,mM1) = Meff(gS3XC12H25,mM1) + (-1_WP)
  Meff(gSXC12H25,mM1) = Meff(gSXC12H25,mM1) + (-1_WP)
  Meff(gC12OOH,mM1) = Meff(gC12OOH,mM1) + (-1_WP)
  Meff(gO2C12H24OOH,mM1) = Meff(gO2C12H24OOH,mM1) + (-1_WP)

  Meff(1:npS,mM2) = Meff(1:npS,mM2) + 1.0_WP
  Meff(gH2,mM2) = Meff(gH2,mM2) + (1_WP)
  Meff(gH2O,mM2) = Meff(gH2O,mM2) + (5_WP)
  Meff(gCH2,mM2) = Meff(gCH2,mM2) + (-1_WP)
  Meff(gHCO,mM2) = Meff(gHCO,mM2) + (-1_WP)
  Meff(gCO2,mM2) = Meff(gCO2,mM2) + (2.6_WP)
  Meff(gCH2D,mM2) = Meff(gCH2D,mM2) + (-1_WP)
  Meff(gCO,mM2) = Meff(gCO,mM2) + (0.75_WP)
  Meff(gCH3O,mM2) = Meff(gCH3O,mM2) + (-1_WP)
  Meff(gC2H3,mM2) = Meff(gC2H3,mM2) + (-1_WP)
  Meff(gCH2CHO,mM2) = Meff(gCH2CHO,mM2) + (-1_WP)
  Meff(gC2H5,mM2) = Meff(gC2H5,mM2) + (-1_WP)
  Meff(gAXC3H5,mM2) = Meff(gAXC3H5,mM2) + (-1_WP)
  Meff(gC2H3CHO,mM2) = Meff(gC2H3CHO,mM2) + (-1_WP)
  Meff(gNXC3H7,mM2) = Meff(gNXC3H7,mM2) + (-1_WP)
  Meff(gC4H7,mM2) = Meff(gC4H7,mM2) + (-1_WP)
  Meff(gPXC4H9,mM2) = Meff(gPXC4H9,mM2) + (-1_WP)
  Meff(gPXC5H11,mM2) = Meff(gPXC5H11,mM2) + (-1_WP)
  Meff(gPXC7H15,mM2) = Meff(gPXC7H15,mM2) + (-1_WP)
  Meff(gPXC12H25,mM2) = Meff(gPXC12H25,mM2) + (-1_WP)
  Meff(gS3XC12H25,mM2) = Meff(gS3XC12H25,mM2) + (-1_WP)
  Meff(gSXC12H25,mM2) = Meff(gSXC12H25,mM2) + (-1_WP)
  Meff(gC12OOH,mM2) = Meff(gC12OOH,mM2) + (-1_WP)
  Meff(gO2C12H24OOH,mM2) = Meff(gO2C12H24OOH,mM2) + (-1_WP)

  Meff(1:npS,mM13) = Meff(1:npS,mM13) + 1.0_WP
  Meff(gH2,mM13) = Meff(gH2,mM13) + (1_WP)
  Meff(gH2O,mM13) = Meff(gH2O,mM13) + (5_WP)
  Meff(gCH2,mM13) = Meff(gCH2,mM13) + (-1_WP)
  Meff(gHCO,mM13) = Meff(gHCO,mM13) + (-1_WP)
  Meff(gCO2,mM13) = Meff(gCO2,mM13) + (1_WP)
  Meff(gCH2D,mM13) = Meff(gCH2D,mM13) + (-1_WP)
  Meff(gCO,mM13) = Meff(gCO,mM13) + (0.5_WP)
  Meff(gC2H6,mM13) = Meff(gC2H6,mM13) + (2_WP)
  Meff(gCH4,mM13) = Meff(gCH4,mM13) + (1_WP)
  Meff(gCH3O,mM13) = Meff(gCH3O,mM13) + (-1_WP)
  Meff(gC2H3,mM13) = Meff(gC2H3,mM13) + (-1_WP)
  Meff(gCH2CHO,mM13) = Meff(gCH2CHO,mM13) + (-1_WP)
  Meff(gC2H5,mM13) = Meff(gC2H5,mM13) + (-1_WP)
  Meff(gAXC3H5,mM13) = Meff(gAXC3H5,mM13) + (-1_WP)
  Meff(gC2H3CHO,mM13) = Meff(gC2H3CHO,mM13) + (-1_WP)
  Meff(gNXC3H7,mM13) = Meff(gNXC3H7,mM13) + (-1_WP)
  Meff(gC4H7,mM13) = Meff(gC4H7,mM13) + (-1_WP)
  Meff(gPXC4H9,mM13) = Meff(gPXC4H9,mM13) + (-1_WP)
  Meff(gPXC5H11,mM13) = Meff(gPXC5H11,mM13) + (-1_WP)
  Meff(gPXC7H15,mM13) = Meff(gPXC7H15,mM13) + (-1_WP)
  Meff(gPXC12H25,mM13) = Meff(gPXC12H25,mM13) + (-1_WP)
  Meff(gS3XC12H25,mM13) = Meff(gS3XC12H25,mM13) + (-1_WP)
  Meff(gSXC12H25,mM13) = Meff(gSXC12H25,mM13) + (-1_WP)
  Meff(gC12OOH,mM13) = Meff(gC12OOH,mM13) + (-1_WP)
  Meff(gO2C12H24OOH,mM13) = Meff(gO2C12H24OOH,mM13) + (-1_WP)

  Meff(1:npS,mM14) = Meff(1:npS,mM14) + 1.0_WP
  Meff(gH2,mM14) = Meff(gH2,mM14) + (1_WP)
  Meff(gH2O,mM14) = Meff(gH2O,mM14) + (5_WP)
  Meff(gCH2,mM14) = Meff(gCH2,mM14) + (-1_WP)
  Meff(gHCO,mM14) = Meff(gHCO,mM14) + (-1_WP)
  Meff(gCO2,mM14) = Meff(gCO2,mM14) + (1_WP)
  Meff(gCH2D,mM14) = Meff(gCH2D,mM14) + (-1_WP)
  Meff(gCO,mM14) = Meff(gCO,mM14) + (0.5_WP)
  Meff(gC2H6,mM14) = Meff(gC2H6,mM14) + (2_WP)
  Meff(gCH4,mM14) = Meff(gCH4,mM14) + (1_WP)
  Meff(gCH3O,mM14) = Meff(gCH3O,mM14) + (-1_WP)
  Meff(gC2H3,mM14) = Meff(gC2H3,mM14) + (-1_WP)
  Meff(gCH2CHO,mM14) = Meff(gCH2CHO,mM14) + (-1_WP)
  Meff(gC2H5,mM14) = Meff(gC2H5,mM14) + (-1_WP)
  Meff(gAXC3H5,mM14) = Meff(gAXC3H5,mM14) + (-1_WP)
  Meff(gC2H3CHO,mM14) = Meff(gC2H3CHO,mM14) + (-1_WP)
  Meff(gNXC3H7,mM14) = Meff(gNXC3H7,mM14) + (-1_WP)
  Meff(gC4H7,mM14) = Meff(gC4H7,mM14) + (-1_WP)
  Meff(gPXC4H9,mM14) = Meff(gPXC4H9,mM14) + (-1_WP)
  Meff(gPXC5H11,mM14) = Meff(gPXC5H11,mM14) + (-1_WP)
  Meff(gPXC7H15,mM14) = Meff(gPXC7H15,mM14) + (-1_WP)
  Meff(gPXC12H25,mM14) = Meff(gPXC12H25,mM14) + (-1_WP)
  Meff(gS3XC12H25,mM14) = Meff(gS3XC12H25,mM14) + (-1_WP)
  Meff(gSXC12H25,mM14) = Meff(gSXC12H25,mM14) + (-1_WP)
  Meff(gC12OOH,mM14) = Meff(gC12OOH,mM14) + (-1_WP)
  Meff(gO2C12H24OOH,mM14) = Meff(gO2C12H24OOH,mM14) + (-1_WP)

  Meff(1:npS,mM3) = Meff(1:npS,mM3) + 1.0_WP
  Meff(gH2O,mM3) = Meff(gH2O,mM3) + (10.89_WP)
  Meff(gO2,mM3) = Meff(gO2,mM3) + (-0.15_WP)
  Meff(gCH2,mM3) = Meff(gCH2,mM3) + (-1_WP)
  Meff(gHCO,mM3) = Meff(gHCO,mM3) + (-1_WP)
  Meff(gCO2,mM3) = Meff(gCO2,mM3) + (1.18_WP)
  Meff(gCH2D,mM3) = Meff(gCH2D,mM3) + (-1_WP)
  Meff(gCO,mM3) = Meff(gCO,mM3) + (0.0900000000000001_WP)
  Meff(gCH3O,mM3) = Meff(gCH3O,mM3) + (-1_WP)
  Meff(gC2H3,mM3) = Meff(gC2H3,mM3) + (-1_WP)
  Meff(gCH2CHO,mM3) = Meff(gCH2CHO,mM3) + (-1_WP)
  Meff(gC2H5,mM3) = Meff(gC2H5,mM3) + (-1_WP)
  Meff(gAXC3H5,mM3) = Meff(gAXC3H5,mM3) + (-1_WP)
  Meff(gC2H3CHO,mM3) = Meff(gC2H3CHO,mM3) + (-1_WP)
  Meff(gNXC3H7,mM3) = Meff(gNXC3H7,mM3) + (-1_WP)
  Meff(gC4H7,mM3) = Meff(gC4H7,mM3) + (-1_WP)
  Meff(gPXC4H9,mM3) = Meff(gPXC4H9,mM3) + (-1_WP)
  Meff(gPXC5H11,mM3) = Meff(gPXC5H11,mM3) + (-1_WP)
  Meff(gPXC7H15,mM3) = Meff(gPXC7H15,mM3) + (-1_WP)
  Meff(gPXC12H25,mM3) = Meff(gPXC12H25,mM3) + (-1_WP)
  Meff(gS3XC12H25,mM3) = Meff(gS3XC12H25,mM3) + (-1_WP)
  Meff(gSXC12H25,mM3) = Meff(gSXC12H25,mM3) + (-1_WP)
  Meff(gC12OOH,mM3) = Meff(gC12OOH,mM3) + (-1_WP)
  Meff(gO2C12H24OOH,mM3) = Meff(gO2C12H24OOH,mM3) + (-1_WP)

  Meff(1:npS,mM11) = Meff(1:npS,mM11) + 1.0_WP
  Meff(gH2,mM11) = Meff(gH2,mM11) + (1_WP)
  Meff(gH2O,mM11) = Meff(gH2O,mM11) + (5_WP)
  Meff(gCH2,mM11) = Meff(gCH2,mM11) + (-1_WP)
  Meff(gHCO,mM11) = Meff(gHCO,mM11) + (-1_WP)
  Meff(gCO2,mM11) = Meff(gCO2,mM11) + (1_WP)
  Meff(gCH2D,mM11) = Meff(gCH2D,mM11) + (-1_WP)
  Meff(gCO,mM11) = Meff(gCO,mM11) + (0.5_WP)
  Meff(gC2H6,mM11) = Meff(gC2H6,mM11) + (2_WP)
  Meff(gCH4,mM11) = Meff(gCH4,mM11) + (1_WP)
  Meff(gCH3O,mM11) = Meff(gCH3O,mM11) + (-1_WP)
  Meff(gC2H3,mM11) = Meff(gC2H3,mM11) + (-1_WP)
  Meff(gCH2CHO,mM11) = Meff(gCH2CHO,mM11) + (-1_WP)
  Meff(gC2H5,mM11) = Meff(gC2H5,mM11) + (-1_WP)
  Meff(gAXC3H5,mM11) = Meff(gAXC3H5,mM11) + (-1_WP)
  Meff(gC2H3CHO,mM11) = Meff(gC2H3CHO,mM11) + (-1_WP)
  Meff(gNXC3H7,mM11) = Meff(gNXC3H7,mM11) + (-1_WP)
  Meff(gC4H7,mM11) = Meff(gC4H7,mM11) + (-1_WP)
  Meff(gPXC4H9,mM11) = Meff(gPXC4H9,mM11) + (-1_WP)
  Meff(gPXC5H11,mM11) = Meff(gPXC5H11,mM11) + (-1_WP)
  Meff(gPXC7H15,mM11) = Meff(gPXC7H15,mM11) + (-1_WP)
  Meff(gPXC12H25,mM11) = Meff(gPXC12H25,mM11) + (-1_WP)
  Meff(gS3XC12H25,mM11) = Meff(gS3XC12H25,mM11) + (-1_WP)
  Meff(gSXC12H25,mM11) = Meff(gSXC12H25,mM11) + (-1_WP)
  Meff(gC12OOH,mM11) = Meff(gC12OOH,mM11) + (-1_WP)
  Meff(gO2C12H24OOH,mM11) = Meff(gO2C12H24OOH,mM11) + (-1_WP)

  Meff(1:npS,mM9) = Meff(1:npS,mM9) + 1.0_WP
  Meff(gH2,mM9) = Meff(gH2,mM9) + (1_WP)
  Meff(gH2O,mM9) = Meff(gH2O,mM9) + (5_WP)
  Meff(gCH2,mM9) = Meff(gCH2,mM9) + (-1_WP)
  Meff(gHCO,mM9) = Meff(gHCO,mM9) + (-1_WP)
  Meff(gCO2,mM9) = Meff(gCO2,mM9) + (1_WP)
  Meff(gCH2D,mM9) = Meff(gCH2D,mM9) + (-1_WP)
  Meff(gCO,mM9) = Meff(gCO,mM9) + (0.5_WP)
  Meff(gC2H6,mM9) = Meff(gC2H6,mM9) + (2_WP)
  Meff(gCH4,mM9) = Meff(gCH4,mM9) + (1_WP)
  Meff(gC2H4,mM9) = Meff(gC2H4,mM9) + (2_WP)
  Meff(gCH3O,mM9) = Meff(gCH3O,mM9) + (-1_WP)
  Meff(gC2H2,mM9) = Meff(gC2H2,mM9) + (2_WP)
  Meff(gC2H3,mM9) = Meff(gC2H3,mM9) + (-1_WP)
  Meff(gCH2CHO,mM9) = Meff(gCH2CHO,mM9) + (-1_WP)
  Meff(gC2H5,mM9) = Meff(gC2H5,mM9) + (-1_WP)
  Meff(gAXC3H5,mM9) = Meff(gAXC3H5,mM9) + (-1_WP)
  Meff(gC2H3CHO,mM9) = Meff(gC2H3CHO,mM9) + (-1_WP)
  Meff(gNXC3H7,mM9) = Meff(gNXC3H7,mM9) + (-1_WP)
  Meff(gC4H7,mM9) = Meff(gC4H7,mM9) + (-1_WP)
  Meff(gPXC4H9,mM9) = Meff(gPXC4H9,mM9) + (-1_WP)
  Meff(gPXC5H11,mM9) = Meff(gPXC5H11,mM9) + (-1_WP)
  Meff(gPXC7H15,mM9) = Meff(gPXC7H15,mM9) + (-1_WP)
  Meff(gPXC12H25,mM9) = Meff(gPXC12H25,mM9) + (-1_WP)
  Meff(gS3XC12H25,mM9) = Meff(gS3XC12H25,mM9) + (-1_WP)
  Meff(gSXC12H25,mM9) = Meff(gSXC12H25,mM9) + (-1_WP)
  Meff(gC12OOH,mM9) = Meff(gC12OOH,mM9) + (-1_WP)
  Meff(gO2C12H24OOH,mM9) = Meff(gO2C12H24OOH,mM9) + (-1_WP)


  return
end subroutine fcmech_thirdbody_efficiency

! ===================== !
! Cp and H computations !
! ===================== !
subroutine fcmech_thermodata(T)
  use fcmech
  implicit none

  ! Input variable
  real(WP) :: T

  ! All species with default medium temperature of 1000K
  if (T.gt.1000) then

    hsp(gN2) = 296.728765167737 * ( T * ((2.92664000e+00_WP) + T * ((0.0007439884_WP) + T * ((-1.89492e-07_WP) + T * ((2.5242595e-11_WP) + T * ((-1.3506702e-15_WP)))))) + (-9.22797700e+02_WP))
    Cpsp(gN2) = 296.728765167737 * ((2.92664000e+00_WP) + T * ((1.48797680e-03_WP) + T * ((-5.68476000e-07_WP) + T * ((1.00970380e-10_WP) + T * (-6.75335100e-15_WP)))))

    hsp(gO) = 519.64625 * ( T * ((2.56942078e+00_WP) + T * ((-4.298705685e-05_WP) + T * ((1.39828196333333e-08_WP) + T * ((-2.504444975e-12_WP) + T * ((2.45667382e-16_WP)))))) + (2.92175791e+04_WP))
    Cpsp(gO) = 519.64625 * ((2.56942078e+00_WP) + T * ((-8.59741137e-05_WP) + T * ((4.19484589e-08_WP) + T * ((-1.00177799e-11_WP) + T * (1.22833691e-15_WP)))))

    hsp(gH2) = 4124.17658730159 * ( T * ((3.33727920e+00_WP) + T * ((-2.470123655e-05_WP) + T * ((1.66485592666667e-07_WP) + T * ((-4.48915985e-11_WP) + T * ((4.00510752e-15_WP)))))) + (-9.50158922e+02_WP))
    Cpsp(gH2) = 4124.17658730159 * ((3.33727920e+00_WP) + T * ((-4.94024731e-05_WP) + T * ((4.99456778e-07_WP) + T * ((-1.79566394e-10_WP) + T * (2.00255376e-14_WP)))))

    hsp(gH) = 8248.35317460317 * ( T * ((2.50000001e+00_WP) + T * ((-1.154214865e-11_WP) + T * ((5.38539826666667e-15_WP) + T * ((-1.1837880875e-18_WP) + T * ((9.96394714e-23_WP)))))) + (2.54736599e+04_WP))
    Cpsp(gH) = 8248.35317460317 * ((2.50000001e+00_WP) + T * ((-2.30842973e-11_WP) + T * ((1.61561948e-14_WP) + T * ((-4.73515235e-18_WP) + T * (4.98197357e-22_WP)))))

    hsp(gOH) = 488.848777046096 * ( T * ((2.86472886e+00_WP) + T * ((0.00052825224_WP) + T * ((-8.63609193333333e-08_WP) + T * ((7.63046685e-12_WP) + T * ((-2.66391752e-16_WP)))))) + (3.71885774e+03_WP))
    Cpsp(gOH) = 488.848777046096 * ((2.86472886e+00_WP) + T * ((1.05650448e-03_WP) + T * ((-2.59082758e-07_WP) + T * ((3.05218674e-11_WP) + T * (-1.33195876e-15_WP)))))

    hsp(gH2O) = 461.497557726465 * ( T * ((3.03399249e+00_WP) + T * ((0.00108845902_WP) + T * ((-5.46908393333333e-08_WP) + T * ((-2.426049675e-11_WP) + T * ((3.36401984e-15_WP)))))) + (-3.00042971e+04_WP))
    Cpsp(gH2O) = 461.497557726465 * ((3.03399249e+00_WP) + T * ((2.17691804e-03_WP) + T * ((-1.64072518e-07_WP) + T * ((-9.70419870e-11_WP) + T * (1.68200992e-14_WP)))))

    hsp(gH2O2) = 244.424388523048 * ( T * ((4.16500285e+00_WP) + T * ((0.00245415847_WP) + T * ((-6.33797416666667e-07_WP) + T * ((9.27964965e-11_WP) + T * ((-5.7581661e-15_WP)))))) + (-1.78617877e+04_WP))
    Cpsp(gH2O2) = 244.424388523048 * ((4.16500285e+00_WP) + T * ((4.90831694e-03_WP) + T * ((-1.90139225e-06_WP) + T * ((3.71185986e-10_WP) + T * (-2.87908305e-14_WP)))))

    hsp(gO2) = 259.823125 * ( T * ((3.28253784e+00_WP) + T * ((0.00074154377_WP) + T * ((-2.52655556333333e-07_WP) + T * ((5.236763875e-11_WP) + T * ((-4.33435588e-15_WP)))))) + (-1.08845772e+03_WP))
    Cpsp(gO2) = 259.823125 * ((3.28253784e+00_WP) + T * ((1.48308754e-03_WP) + T * ((-7.57966669e-07_WP) + T * ((2.09470555e-10_WP) + T * (-2.16717794e-14_WP)))))

    hsp(gHO2) = 251.888633058652 * ( T * ((4.01721090e+00_WP) + T * ((0.001119910065_WP) + T * ((-2.11219383333333e-07_WP) + T * ((2.85615925e-11_WP) + T * ((-2.1581707e-15_WP)))))) + (1.11856713e+02_WP))
    Cpsp(gHO2) = 251.888633058652 * ((4.01721090e+00_WP) + T * ((2.23982013e-03_WP) + T * ((-6.33658150e-07_WP) + T * ((1.14246370e-10_WP) + T * (-1.07908535e-14_WP)))))

    hsp(gCH2O) = 276.904682608406 * ( T * ((1.76069008e+00_WP) + T * ((0.00460000041_WP) + T * ((-1.47419604333333e-06_WP) + T * ((2.5160303e-10_WP) + T * ((-1.76771128e-14_WP)))))) + (-1.39958323e+04_WP))
    Cpsp(gCH2O) = 276.904682608406 * ((1.76069008e+00_WP) + T * ((9.20000082e-03_WP) + T * ((-4.42258813e-06_WP) + T * ((1.00641212e-09_WP) + T * (-8.83855640e-14_WP)))))

    hsp(gCO2) = 188.919336514429 * ( T * ((3.85746029e+00_WP) + T * ((0.00220718513_WP) + T * ((-7.38271346666667e-07_WP) + T * ((1.30872547e-10_WP) + T * ((-9.44168328e-15_WP)))))) + (-4.87591660e+04_WP))
    Cpsp(gCO2) = 188.919336514429 * ((3.85746029e+00_WP) + T * ((4.41437026e-03_WP) + T * ((-2.21481404e-06_WP) + T * ((5.23490188e-10_WP) + T * (-4.72084164e-14_WP)))))

    hsp(gCH3) = 553.035785552747 * ( T * ((2.28571772e+00_WP) + T * ((0.003619950185_WP) + T * ((-9.95714493333333e-07_WP) + T * ((1.48921161e-10_WP) + T * ((-9.34308788e-15_WP)))))) + (1.67755843e+04_WP))
    Cpsp(gCH3) = 553.035785552747 * ((2.28571772e+00_WP) + T * ((7.23990037e-03_WP) + T * ((-2.98714348e-06_WP) + T * ((5.95684644e-10_WP) + T * (-4.67154394e-14_WP)))))

    hsp(gCO) = 296.834701892181 * ( T * ((2.71518561e+00_WP) + T * ((0.001031263715_WP) + T * ((-3.32941923666667e-07_WP) + T * ((5.7513252e-11_WP) + T * ((-4.07295432e-15_WP)))))) + (-1.41518724e+04_WP))
    Cpsp(gCO) = 296.834701892181 * ((2.71518561e+00_WP) + T * ((2.06252743e-03_WP) + T * ((-9.98825771e-07_WP) + T * ((2.30053008e-10_WP) + T * (-2.03647716e-14_WP)))))

    hsp(gC2H6) = 276.517892776374 * ( T * ((1.07188150e+00_WP) + T * ((0.01084263385_WP) + T * ((-3.3418689e-06_WP) + T * ((5.535300025e-10_WP) + T * ((-3.8000578e-14_WP)))))) + (-1.14263932e+04_WP))
    Cpsp(gC2H6) = 276.517892776374 * ((1.07188150e+00_WP) + T * ((2.16852677e-02_WP) + T * ((-1.00256067e-05_WP) + T * ((2.21412001e-09_WP) + T * (-1.90002890e-13_WP)))))

    hsp(gCH4) = 518.285749906495 * ( T * ((7.48514950e-02_WP) + T * ((0.00669547335_WP) + T * ((-1.91095269666667e-06_WP) + T * ((3.057313375e-10_WP) + T * ((-2.0363046e-14_WP)))))) + (-9.46834459e+03_WP))
    Cpsp(gCH4) = 518.285749906495 * ((7.48514950e-02_WP) + T * ((1.33909467e-02_WP) + T * ((-5.73285809e-06_WP) + T * ((1.22292535e-09_WP) + T * (-1.01815230e-13_WP)))))

    hsp(gC2H4) = 296.390275203194 * ( T * ((2.03611116e+00_WP) + T * ((0.00732270755_WP) + T * ((-2.23692638333333e-06_WP) + T * ((3.680573075e-10_WP) + T * ((-2.51412122e-14_WP)))))) + (4.93988614e+03_WP))
    Cpsp(gC2H4) = 296.390275203194 * ((2.03611116e+00_WP) + T * ((1.46454151e-02_WP) + T * ((-6.71077915e-06_WP) + T * ((1.47222923e-09_WP) + T * (-1.25706061e-13_WP)))))

    hsp(gC2H2) = 319.340144415425 * ( T * ((4.14756964e+00_WP) + T * ((0.00298083332_WP) + T * ((-7.9098284e-07_WP) + T * ((1.1685304275e-10_WP) + T * ((-7.22470426e-15_WP)))))) + (2.59359992e+04_WP))
    Cpsp(gC2H2) = 319.340144415425 * ((4.14756964e+00_WP) + T * ((5.96166664e-03_WP) + T * ((-2.37294852e-06_WP) + T * ((4.67412171e-10_WP) + T * (-3.61235213e-14_WP)))))

    hsp(gC3H6) = 197.593516802129 * ( T * ((6.73225700e+00_WP) + T * ((0.00745417_WP) + T * ((-1.64996633333333e-06_WP) + T * ((1.8030055e-10_WP) + T * ((-7.532408e-15_WP)))))) + (-9.23570300e+02_WP))
    Cpsp(gC3H6) = 197.593516802129 * ((6.73225700e+00_WP) + T * ((1.49083400e-02_WP) + T * ((-4.94989900e-06_WP) + T * ((7.21202200e-10_WP) + T * (-3.76620400e-14_WP)))))

    hsp(gC4H81) = 148.195137601597 * ( T * ((2.05358410e+00_WP) + T * ((0.0171752535_WP) + T * ((-5.294399e-06_WP) + T * ((8.2724155e-10_WP) + T * ((-5.072209e-14_WP)))))) + (-2.13972310e+03_WP))
    Cpsp(gC4H81) = 148.195137601597 * ((2.05358410e+00_WP) + T * ((3.43505070e-02_WP) + T * ((-1.58831970e-05_WP) + T * ((3.30896620e-09_WP) + T * (-2.53610450e-13_WP)))))

    hsp(gC5H10) = 118.556110081278 * ( T * ((3.98580522e+00_WP) + T * ((0.0206214993_WP) + T * ((-6.1463499e-06_WP) + T * ((7.653881025e-10_WP) + T * ((0_WP)))))) + (-5.70112071e+03_WP))
    Cpsp(gC5H10) = 118.556110081278 * ((3.98580522e+00_WP) + T * ((4.12429986e-02_WP) + T * ((-1.84390497e-05_WP) + T * ((3.06155241e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC6H12) = 98.7967584010647 * ( T * ((4.97075871e+00_WP) + T * ((0.02483186605_WP) + T * ((-7.4315839e-06_WP) + T * ((9.2900545e-10_WP) + T * ((0_WP)))))) + (-8.89572273e+03_WP))
    Cpsp(gC6H12) = 98.7967584010647 * ((4.97075871e+00_WP) + T * ((4.96637321e-02_WP) + T * ((-2.22947517e-05_WP) + T * ((3.71602180e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC7H14) = 84.6829357723412 * ( T * ((5.97612676e+00_WP) + T * ((0.0290245262_WP) + T * ((-8.70960413333333e-06_WP) + T * ((1.091506865e-09_WP) + T * ((0_WP)))))) + (-1.20977632e+04_WP))
    Cpsp(gC7H14) = 84.6829357723412 * ((5.97612676e+00_WP) + T * ((5.80490524e-02_WP) + T * ((-2.61288124e-05_WP) + T * ((4.36602746e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC8H16) = 74.0975688007985 * ( T * ((6.92039805e+00_WP) + T * ((0.03327621255_WP) + T * ((-1.00136485333333e-05_WP) + T * ((1.258145255e-09_WP) + T * ((0_WP)))))) + (-1.52768448e+04_WP))
    Cpsp(gC8H16) = 74.0975688007985 * ((6.92039805e+00_WP) + T * ((6.65524251e-02_WP) + T * ((-3.00409456e-05_WP) + T * ((5.03258102e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC9H18) = 65.8645056007098 * ( T * ((7.88457953e+00_WP) + T * ((0.03750948785_WP) + T * ((-1.13093176e-05_WP) + T * ((1.4234199425e-09_WP) + T * ((0_WP)))))) + (-1.84645629e+04_WP))
    Cpsp(gC9H18) = 65.8645056007098 * ((7.88457953e+00_WP) + T * ((7.50189757e-02_WP) + T * ((-3.39279528e-05_WP) + T * ((5.69367977e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC10H20) = 59.2780550406388 * ( T * ((8.84741308e+00_WP) + T * ((0.0417443665_WP) + T * ((-1.26057595333333e-05_WP) + T * ((1.58882176e-09_WP) + T * ((0_WP)))))) + (-2.16518855e+04_WP))
    Cpsp(gC10H20) = 59.2780550406388 * ((8.84741308e+00_WP) + T * ((8.34887330e-02_WP) + T * ((-3.78172786e-05_WP) + T * ((6.35528704e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC12H25O2) = 41.2991257699185 * ( T * ((2.84782000e+01_WP) + T * ((0.02687695_WP) + T * ((-5.6062e-06_WP) + T * ((6.284175e-10_WP) + T * ((-2.94416e-14_WP)))))) + (-3.74118000e+04_WP))
    Cpsp(gC12H25O2) = 41.2991257699185 * ((2.84782000e+01_WP) + T * ((5.37539000e-02_WP) + T * ((-1.68186000e-05_WP) + T * ((2.51367000e-09_WP) + T * (-1.47208000e-13_WP)))))

    hsp(gNXC12H26) = 48.8137006246771 * ( T * ((9.97283422e+00_WP) + T * ((0.053807648_WP) + T * ((-1.63000417e-05_WP) + T * ((2.0597418575e-09_WP) + T * ((0_WP)))))) + (-4.31954165e+04_WP))
    Cpsp(gNXC12H26) = 48.8137006246771 * ((9.97283422e+00_WP) + T * ((1.07615296e-01_WP) + T * ((-4.89001251e-05_WP) + T * ((8.23896743e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gOC12H23OOH) = 38.4367949998151 * ( T * ((2.36731000e+01_WP) + T * ((0.0308196_WP) + T * ((-6.99453333333333e-06_WP) + T * ((8.32915e-10_WP) + T * ((-4.0718e-14_WP)))))) + (-7.18258000e+04_WP))
    Cpsp(gOC12H23OOH) = 38.4367949998151 * ((2.36731000e+01_WP) + T * ((6.16392000e-02_WP) + T * ((-2.09836000e-05_WP) + T * ((3.33166000e-09_WP) + T * (-2.03590000e-13_WP)))))

    hsp(gCH2) = 592.780550406388 * ( T * ((2.87410113e+00_WP) + T * ((0.00182819646_WP) + T * ((-4.69648656666667e-07_WP) + T * ((6.504488725e-11_WP) + T * ((-3.75455134e-15_WP)))))) + (4.62636040e+04_WP))
    Cpsp(gCH2) = 592.780550406388 * ((2.87410113e+00_WP) + T * ((3.65639292e-03_WP) + T * ((-1.40894597e-06_WP) + T * ((2.60179549e-10_WP) + T * (-1.87727567e-14_WP)))))

    hsp(gHCO) = 286.523537114894 * ( T * ((2.77217438e+00_WP) + T * ((0.00247847763_WP) + T * ((-8.28152043333333e-07_WP) + T * ((1.472904445e-10_WP) + T * ((-1.067017422e-14_WP)))))) + (4.01191815e+03_WP))
    Cpsp(gHCO) = 286.523537114894 * ((2.77217438e+00_WP) + T * ((4.95695526e-03_WP) + T * ((-2.48445613e-06_WP) + T * ((5.89161778e-10_WP) + T * (-5.33508711e-14_WP)))))

    hsp(gCH2D) = 592.780550406388 * ( T * ((2.29203842e+00_WP) + T * ((0.002327943185_WP) + T * ((-6.70639823333333e-07_WP) + T * ((1.044765e-10_WP) + T * ((-6.7943273e-15_WP)))))) + (5.09259997e+04_WP))
    Cpsp(gCH2D) = 592.780550406388 * ((2.29203842e+00_WP) + T * ((4.65588637e-03_WP) + T * ((-2.01191947e-06_WP) + T * ((4.17906000e-10_WP) + T * (-3.39716365e-14_WP)))))

    hsp(gCH3O) = 267.910678610556 * ( T * ((4.75779238e+00_WP) + T * ((0.00372071237_WP) + T * ((-8.99017253333333e-07_WP) + T * ((1.09522626e-10_WP) + T * ((-5.27074196e-15_WP)))))) + (3.78111940e+02_WP))
    Cpsp(gCH3O) = 267.910678610556 * ((4.75779238e+00_WP) + T * ((7.44142474e-03_WP) + T * ((-2.69705176e-06_WP) + T * ((4.38090504e-10_WP) + T * (-2.63537098e-14_WP)))))

    hsp(gC2H3) = 307.437509244195 * ( T * ((3.01672400e+00_WP) + T * ((0.0051651146_WP) + T * ((-1.56027449666667e-06_WP) + T * ((2.5440822e-10_WP) + T * ((-1.725214082e-14_WP)))))) + (3.46128739e+04_WP))
    Cpsp(gC2H3) = 307.437509244195 * ((3.01672400e+00_WP) + T * ((1.03302292e-02_WP) + T * ((-4.68082349e-06_WP) + T * ((1.01763288e-09_WP) + T * (-8.62607041e-14_WP)))))

    hsp(gCH2CHO) = 193.159093021095 * ( T * ((5.97566990e+00_WP) + T * ((0.0040652957_WP) + T * ((-9.145415e-07_WP) + T * ((1.017576025e-10_WP) + T * ((-4.3520342e-15_WP)))))) + (-9.69500000e+02_WP))
    Cpsp(gCH2CHO) = 193.159093021095 * ((5.97566990e+00_WP) + T * ((8.13059140e-03_WP) + T * ((-2.74362450e-06_WP) + T * ((4.07030410e-10_WP) + T * (-2.17601710e-14_WP)))))

    hsp(gC2H5) = 286.109428768066 * ( T * ((1.95465642e+00_WP) + T * ((0.0086986361_WP) + T * ((-2.66068889333333e-06_WP) + T * ((4.380442225e-10_WP) + T * ((-2.99283152e-14_WP)))))) + (1.28575200e+04_WP))
    Cpsp(gC2H5) = 286.109428768066 * ((1.95465642e+00_WP) + T * ((1.73972722e-02_WP) + T * ((-7.98206668e-06_WP) + T * ((1.75217689e-09_WP) + T * (-1.49641576e-13_WP)))))

    hsp(gAXC3H5) = 202.443145848551 * ( T * ((6.50078770e+00_WP) + T * ((0.0071623655_WP) + T * ((-1.89272106666667e-06_WP) + T * ((2.77020025e-10_WP) + T * ((-1.80727774e-14_WP)))))) + (1.74824490e+04_WP))
    Cpsp(gAXC3H5) = 202.443145848551 * ((6.50078770e+00_WP) + T * ((1.43247310e-02_WP) + T * ((-5.67816320e-06_WP) + T * ((1.10808010e-09_WP) + T * (-9.03638870e-14_WP)))))

    hsp(gC2H3CHO) = 148.306161035996 * ( T * ((5.81118680e+00_WP) + T * ((0.008557128_WP) + T * ((-2.49447203333333e-06_WP) + T * ((3.56306225e-10_WP) + T * ((-1.83493682e-14_WP)))))) + (-1.07840540e+04_WP))
    Cpsp(gC2H3CHO) = 148.306161035996 * ((5.81118680e+00_WP) + T * ((1.71142560e-02_WP) + T * ((-7.48341610e-06_WP) + T * ((1.42522490e-09_WP) + T * (-9.17468410e-14_WP)))))

    hsp(gNXC3H7) = 192.970802580885 * ( T * ((7.70974790e+00_WP) + T * ((0.0080157425_WP) + T * ((-1.75734126666667e-06_WP) + T * ((1.8972088e-10_WP) + T * ((-7.7725438e-15_WP)))))) + (7.97622360e+03_WP))
    Cpsp(gNXC3H7) = 192.970802580885 * ((7.70974790e+00_WP) + T * ((1.60314850e-02_WP) + T * ((-5.27202380e-06_WP) + T * ((7.58883520e-10_WP) + T * (-3.88627190e-14_WP)))))

    hsp(gC4H7) = 150.906417888776 * ( T * ((7.01348350e+00_WP) + T * ((0.011317279_WP) + T * ((-3.084849e-06_WP) + T * ((4.20198175e-10_WP) + T * ((-2.0817234e-14_WP)))))) + (2.09550080e+04_WP))
    Cpsp(gC4H7) = 150.906417888776 * ((7.01348350e+00_WP) + T * ((2.26345580e-02_WP) + T * ((-9.25454700e-06_WP) + T * ((1.68079270e-09_WP) + T * (-1.04086170e-13_WP)))))

    hsp(gPXC4H9) = 145.579562964001 * ( T * ((8.68223950e+00_WP) + T * ((0.0118455355_WP) + T * ((-2.53162883333333e-06_WP) + T * ((1.6606784e-10_WP) + T * ((1.09690272e-14_WP)))))) + (4.96440580e+03_WP))
    Cpsp(gPXC4H9) = 145.579562964001 * ((8.68223950e+00_WP) + T * ((2.36910710e-02_WP) + T * ((-7.59488650e-06_WP) + T * ((6.64271360e-10_WP) + T * (5.48451360e-14_WP)))))

    hsp(gPXC5H11) = 116.876212432174 * ( T * ((3.69396793e+00_WP) + T * ((0.0223250078_WP) + T * ((-6.6932373e-06_WP) + T * ((8.37130865e-10_WP) + T * ((0_WP)))))) + (3.80981997e+03_WP))
    Cpsp(gPXC5H11) = 116.876212432174 * ((3.69396793e+00_WP) + T * ((4.46500156e-02_WP) + T * ((-2.00797119e-05_WP) + T * ((3.34852346e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gPXC7H15) = 83.8223611251134 * ( T * ((5.67638200e+00_WP) + T * ((0.03073142225_WP) + T * ((-9.25845533333333e-06_WP) + T * ((1.1636772275e-09_WP) + T * ((0_WP)))))) + (-2.58110033e+03_WP))
    Cpsp(gPXC7H15) = 83.8223611251134 * ((5.67638200e+00_WP) + T * ((6.14628445e-02_WP) + T * ((-2.77753660e-05_WP) + T * ((4.65470891e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gPXC12H25) = 49.1042995511458 * ( T * ((1.05038381e+01_WP) + T * ((0.0518907195_WP) + T * ((-1.57336506666667e-05_WP) + T * ((1.989558925e-09_WP) + T * ((0_WP)))))) + (-1.85218187e+04_WP))
    Cpsp(gPXC12H25) = 49.1042995511458 * ((1.05038381e+01_WP) + T * ((1.03781439e-01_WP) + T * ((-4.72009520e-05_WP) + T * ((7.95823570e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gS3XC12H25) = 49.1042995511458 * ( T * ((8.53523824e+00_WP) + T * ((0.053674273_WP) + T * ((-1.65429932333333e-05_WP) + T * ((2.1232134275e-09_WP) + T * ((0_WP)))))) + (-1.92995203e+04_WP))
    Cpsp(gS3XC12H25) = 49.1042995511458 * ((8.53523824e+00_WP) + T * ((1.07348546e-01_WP) + T * ((-4.96289797e-05_WP) + T * ((8.49285371e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gSXC12H25) = 49.1042995511458 * ( T * ((8.53523824e+00_WP) + T * ((0.053674273_WP) + T * ((-1.65429932333333e-05_WP) + T * ((2.1232134275e-09_WP) + T * ((0_WP)))))) + (-1.92995203e+04_WP))
    Cpsp(gSXC12H25) = 49.1042995511458 * ((8.53523824e+00_WP) + T * ((1.07348546e-01_WP) + T * ((-4.96289797e-05_WP) + T * ((8.49285371e-09_WP) + T * (0.00000000e+00_WP)))))

    hsp(gC12OOH) = 41.2991257699185 * ( T * ((2.92019000e+01_WP) + T * ((0.02579585_WP) + T * ((-5.24423333333333e-06_WP) + T * ((5.75765e-10_WP) + T * ((-2.6528e-14_WP)))))) + (-3.11192000e+04_WP))
    Cpsp(gC12OOH) = 41.2991257699185 * ((2.92019000e+01_WP) + T * ((5.15917000e-02_WP) + T * ((-1.57327000e-05_WP) + T * ((2.30306000e-09_WP) + T * (-1.32640000e-13_WP)))))

    hsp(gO2C12H24OOH) = 35.6349219955426 * ( T * ((3.50907000e+01_WP) + T * ((0.0255295_WP) + T * ((-5.14483333333333e-06_WP) + T * ((5.615675e-10_WP) + T * ((-2.57802e-14_WP)))))) + (-5.12675000e+04_WP))
    Cpsp(gO2C12H24OOH) = 35.6349219955426 * ((3.50907000e+01_WP) + T * ((5.10590000e-02_WP) + T * ((-1.54345000e-05_WP) + T * ((2.24627000e-09_WP) + T * (-1.28901000e-13_WP)))))


  else 

    hsp(gN2) = 296.728765167737 * ( T * ((3.29867700e+00_WP) + T * ((0.0007041202_WP) + T * ((-1.321074e-06_WP) + T * ((1.41037875e-09_WP) + T * ((-4.889708e-13_WP)))))) + (-1.02089990e+03_WP))
    Cpsp(gN2) = 296.728765167737 * ((3.29867700e+00_WP) + T * ((1.40824040e-03_WP) + T * ((-3.96322200e-06_WP) + T * ((5.64151500e-09_WP) + T * (-2.44485400e-12_WP)))))

    hsp(gO) = 519.64625 * ( T * ((3.16826710e+00_WP) + T * ((-0.00163965942_WP) + T * ((2.21435465333333e-06_WP) + T * ((-1.53201656e-09_WP) + T * ((4.22531942e-13_WP)))))) + (2.91222592e+04_WP))
    Cpsp(gO) = 519.64625 * ((3.16826710e+00_WP) + T * ((-3.27931884e-03_WP) + T * ((6.64306396e-06_WP) + T * ((-6.12806624e-09_WP) + T * (2.11265971e-12_WP)))))

    hsp(gH2) = 4124.17658730159 * ( T * ((2.34433112e+00_WP) + T * ((0.003990260375_WP) + T * ((-6.492717e-06_WP) + T * ((5.03930235e-09_WP) + T * ((-1.475223522e-12_WP)))))) + (-9.17935173e+02_WP))
    Cpsp(gH2) = 4124.17658730159 * ((2.34433112e+00_WP) + T * ((7.98052075e-03_WP) + T * ((-1.94781510e-05_WP) + T * ((2.01572094e-08_WP) + T * (-7.37611761e-12_WP)))))

    hsp(gH) = 8248.35317460317 * ( T * ((2.50000000e+00_WP) + T * ((3.526664095e-13_WP) + T * ((-6.65306546666667e-16_WP) + T * ((5.7520408e-19_WP) + T * ((-1.855464664e-22_WP)))))) + (2.54736599e+04_WP))
    Cpsp(gH) = 8248.35317460317 * ((2.50000000e+00_WP) + T * ((7.05332819e-13_WP) + T * ((-1.99591964e-15_WP) + T * ((2.30081632e-18_WP) + T * (-9.27732332e-22_WP)))))

    hsp(gOH) = 488.848777046096 * ( T * ((4.12530561e+00_WP) + T * ((-0.001612724695_WP) + T * ((2.17588230333333e-06_WP) + T * ((-1.4496341075e-09_WP) + T * ((4.12474758e-13_WP)))))) + (3.38153812e+03_WP))
    Cpsp(gOH) = 488.848777046096 * ((4.12530561e+00_WP) + T * ((-3.22544939e-03_WP) + T * ((6.52764691e-06_WP) + T * ((-5.79853643e-09_WP) + T * (2.06237379e-12_WP)))))

    hsp(gH2O) = 461.497557726465 * ( T * ((4.19864056e+00_WP) + T * ((-0.00101821705_WP) + T * ((2.17346737e-06_WP) + T * ((-1.371992655e-09_WP) + T * ((3.54395634e-13_WP)))))) + (-3.02937267e+04_WP))
    Cpsp(gH2O) = 461.497557726465 * ((4.19864056e+00_WP) + T * ((-2.03643410e-03_WP) + T * ((6.52040211e-06_WP) + T * ((-5.48797062e-09_WP) + T * (1.77197817e-12_WP)))))

    hsp(gH2O2) = 244.424388523048 * ( T * ((4.27611269e+00_WP) + T * ((-0.0002714112085_WP) + T * ((5.5778567e-06_WP) + T * ((-5.394270325e-09_WP) + T * ((1.724908726e-12_WP)))))) + (-1.77025821e+04_WP))
    Cpsp(gH2O2) = 244.424388523048 * ((4.27611269e+00_WP) + T * ((-5.42822417e-04_WP) + T * ((1.67335701e-05_WP) + T * ((-2.15770813e-08_WP) + T * (8.62454363e-12_WP)))))

    hsp(gO2) = 259.823125 * ( T * ((3.78245636e+00_WP) + T * ((-0.00149836708_WP) + T * ((3.28243400333333e-06_WP) + T * ((-2.4203237725e-09_WP) + T * ((6.48745674e-13_WP)))))) + (-1.06394356e+03_WP))
    Cpsp(gO2) = 259.823125 * ((3.78245636e+00_WP) + T * ((-2.99673416e-03_WP) + T * ((9.84730201e-06_WP) + T * ((-9.68129509e-09_WP) + T * (3.24372837e-12_WP)))))

    hsp(gHO2) = 251.888633058652 * ( T * ((4.30179801e+00_WP) + T * ((-0.002374560255_WP) + T * ((7.05276303333333e-06_WP) + T * ((-6.06909735e-09_WP) + T * ((1.858450248e-12_WP)))))) + (2.94808040e+02_WP))
    Cpsp(gHO2) = 251.888633058652 * ((4.30179801e+00_WP) + T * ((-4.74912051e-03_WP) + T * ((2.11582891e-05_WP) + T * ((-2.42763894e-08_WP) + T * (9.29225124e-12_WP)))))

    hsp(gCH2O) = 276.904682608406 * ( T * ((4.79372315e+00_WP) + T * ((-0.004954166845_WP) + T * ((1.24406669333333e-05_WP) + T * ((-9.482131525e-09_WP) + T * ((2.63545304e-12_WP)))))) + (-1.43089567e+04_WP))
    Cpsp(gCH2O) = 276.904682608406 * ((4.79372315e+00_WP) + T * ((-9.90833369e-03_WP) + T * ((3.73220008e-05_WP) + T * ((-3.79285261e-08_WP) + T * (1.31772652e-11_WP)))))

    hsp(gCO2) = 188.919336514429 * ( T * ((2.35677352e+00_WP) + T * ((0.004492298385_WP) + T * ((-2.37452089666667e-06_WP) + T * ((6.14797555e-10_WP) + T * ((-2.87399096e-14_WP)))))) + (-4.83719697e+04_WP))
    Cpsp(gCO2) = 188.919336514429 * ((2.35677352e+00_WP) + T * ((8.98459677e-03_WP) + T * ((-7.12356269e-06_WP) + T * ((2.45919022e-09_WP) + T * (-1.43699548e-13_WP)))))

    hsp(gCH3) = 553.035785552747 * ( T * ((3.67359040e+00_WP) + T * ((0.001005475875_WP) + T * ((1.91007285333333e-06_WP) + T * ((-1.7177935625e-09_WP) + T * ((5.08771468e-13_WP)))))) + (1.64449988e+04_WP))
    Cpsp(gCH3) = 553.035785552747 * ((3.67359040e+00_WP) + T * ((2.01095175e-03_WP) + T * ((5.73021856e-06_WP) + T * ((-6.87117425e-09_WP) + T * (2.54385734e-12_WP)))))

    hsp(gCO) = 296.834701892181 * ( T * ((3.57953347e+00_WP) + T * ((-0.00030517684_WP) + T * ((3.3893811e-07_WP) + T * ((2.26751471e-10_WP) + T * ((-1.808848998e-13_WP)))))) + (-1.43440860e+04_WP))
    Cpsp(gCO) = 296.834701892181 * ((3.57953347e+00_WP) + T * ((-6.10353680e-04_WP) + T * ((1.01681433e-06_WP) + T * ((9.07005884e-10_WP) + T * (-9.04424499e-13_WP)))))

    hsp(gC2H6) = 276.517892776374 * ( T * ((4.29142492e+00_WP) + T * ((-0.00275077135_WP) + T * ((1.99812762666667e-05_WP) + T * ((-1.7711657125e-08_WP) + T * ((5.37371542e-12_WP)))))) + (-1.15222055e+04_WP))
    Cpsp(gC2H6) = 276.517892776374 * ((4.29142492e+00_WP) + T * ((-5.50154270e-03_WP) + T * ((5.99438288e-05_WP) + T * ((-7.08466285e-08_WP) + T * (2.68685771e-11_WP)))))

    hsp(gCH4) = 518.285749906495 * ( T * ((5.14987613e+00_WP) + T * ((-0.0068354894_WP) + T * ((1.63933533e-05_WP) + T * ((-1.211857565e-08_WP) + T * ((3.33387912e-12_WP)))))) + (-1.02466476e+04_WP))
    Cpsp(gCH4) = 518.285749906495 * ((5.14987613e+00_WP) + T * ((-1.36709788e-02_WP) + T * ((4.91800599e-05_WP) + T * ((-4.84743026e-08_WP) + T * (1.66693956e-11_WP)))))

    hsp(gC2H4) = 296.390275203194 * ( T * ((3.95920148e+00_WP) + T * ((-0.003785261235_WP) + T * ((1.90330097333333e-05_WP) + T * ((-1.7289718825e-08_WP) + T * ((5.39768746e-12_WP)))))) + (5.08977593e+03_WP))
    Cpsp(gC2H4) = 296.390275203194 * ((3.95920148e+00_WP) + T * ((-7.57052247e-03_WP) + T * ((5.70990292e-05_WP) + T * ((-6.91588753e-08_WP) + T * (2.69884373e-11_WP)))))

    hsp(gC2H2) = 319.340144415425 * ( T * ((8.08681094e-01_WP) + T * ((0.01168078145_WP) + T * ((-1.18390605e-05_WP) + T * ((7.003810925e-09_WP) + T * ((-1.700145948e-12_WP)))))) + (2.64289807e+04_WP))
    Cpsp(gC2H2) = 319.340144415425 * ((8.08681094e-01_WP) + T * ((2.33615629e-02_WP) + T * ((-3.55171815e-05_WP) + T * ((2.80152437e-08_WP) + T * (-8.50072974e-12_WP)))))

    hsp(gC3H6) = 197.593516802129 * ( T * ((1.49330700e+00_WP) + T * ((0.01046259_WP) + T * ((1.495598e-06_WP) + T * ((-4.17228e-09_WP) + T * ((1.4316292e-12_WP)))))) + (1.07482600e+03_WP))
    Cpsp(gC3H6) = 197.593516802129 * ((1.49330700e+00_WP) + T * ((2.09251800e-02_WP) + T * ((4.48679400e-06_WP) + T * ((-1.66891200e-08_WP) + T * (7.15814600e-12_WP)))))

    hsp(gC4H81) = 148.195137601597 * ( T * ((1.18113800e+00_WP) + T * ((0.01542669_WP) + T * ((1.69550823333333e-06_WP) + T * ((-6.163722e-09_WP) + T * ((2.2220386e-12_WP)))))) + (-1.79040040e+03_WP))
    Cpsp(gC4H81) = 148.195137601597 * ((1.18113800e+00_WP) + T * ((3.08533800e-02_WP) + T * ((5.08652470e-06_WP) + T * ((-2.46548880e-08_WP) + T * (1.11101930e-11_WP)))))

    hsp(gC5H10) = 118.556110081278 * ( T * ((-1.06223481e+00_WP) + T * ((0.0287109147_WP) + T * ((-1.24828963333333e-05_WP) + T * ((3.184124725e-09_WP) + T * ((-3.59219578e-13_WP)))))) + (-4.46546666e+03_WP))
    Cpsp(gC5H10) = 118.556110081278 * ((-1.06223481e+00_WP) + T * ((5.74218294e-02_WP) + T * ((-3.74486890e-05_WP) + T * ((1.27364989e-08_WP) + T * (-1.79609789e-12_WP)))))

    hsp(gC6H12) = 98.7967584010647 * ( T * ((-1.35275205e+00_WP) + T * ((0.0349327713_WP) + T * ((-1.53136007333333e-05_WP) + T * ((3.924183575e-09_WP) + T * ((-4.4259235e-13_WP)))))) + (-7.34368617e+03_WP))
    Cpsp(gC6H12) = 98.7967584010647 * ((-1.35275205e+00_WP) + T * ((6.98655426e-02_WP) + T * ((-4.59408022e-05_WP) + T * ((1.56967343e-08_WP) + T * (-2.21296175e-12_WP)))))

    hsp(gC7H14) = 84.6829357723412 * ( T * ((-1.67720549e+00_WP) + T * ((0.04123058005_WP) + T * ((-1.82168036e-05_WP) + T * ((4.696557575e-09_WP) + T * ((-5.31475966e-13_WP)))))) + (-1.02168601e+04_WP))
    Cpsp(gC7H14) = 84.6829357723412 * ((-1.67720549e+00_WP) + T * ((8.24611601e-02_WP) + T * ((-5.46504108e-05_WP) + T * ((1.87862303e-08_WP) + T * (-2.65737983e-12_WP)))))

    hsp(gC8H16) = 74.0975688007985 * ( T * ((-1.89226915e+00_WP) + T * ((0.04730331785_WP) + T * ((-2.09128507e-05_WP) + T * ((5.378957725e-09_WP) + T * ((-6.05437366e-13_WP)))))) + (-1.31074559e+04_WP))
    Cpsp(gC8H16) = 74.0975688007985 * ((-1.89226915e+00_WP) + T * ((9.46066357e-02_WP) + T * ((-6.27385521e-05_WP) + T * ((2.15158309e-08_WP) + T * (-3.02718683e-12_WP)))))

    hsp(gC9H18) = 65.8645056007098 * ( T * ((-2.16108263e+00_WP) + T * ((0.0534791485_WP) + T * ((-2.36991081333333e-05_WP) + T * ((6.099276925e-09_WP) + T * ((-6.85543094e-13_WP)))))) + (-1.59890847e+04_WP))
    Cpsp(gC9H18) = 65.8645056007098 * ((-2.16108263e+00_WP) + T * ((1.06958297e-01_WP) + T * ((-7.10973244e-05_WP) + T * ((2.43971077e-08_WP) + T * (-3.42771547e-12_WP)))))

    hsp(gC10H20) = 59.2780550406388 * ( T * ((-2.42901688e+00_WP) + T * ((0.059652799_WP) + T * ((-2.64829675e-05_WP) + T * ((6.8184149e-09_WP) + T * ((-7.65436746e-13_WP)))))) + (-1.88708365e+04_WP))
    Cpsp(gC10H20) = 59.2780550406388 * ((-2.42901688e+00_WP) + T * ((1.19305598e-01_WP) + T * ((-7.94489025e-05_WP) + T * ((2.72736596e-08_WP) + T * (-3.82718373e-12_WP)))))

    hsp(gC12H25O2) = 41.2991257699185 * ( T * ((5.31404000e+00_WP) + T * ((0.04469365_WP) + T * ((4.84503333333333e-06_WP) + T * ((-1.873125e-08_WP) + T * ((6.7065e-12_WP)))))) + (-2.98918000e+04_WP))
    Cpsp(gC12H25O2) = 41.2991257699185 * ((5.31404000e+00_WP) + T * ((8.93873000e-02_WP) + T * ((1.45351000e-05_WP) + T * ((-7.49250000e-08_WP) + T * (3.35325000e-11_WP)))))

    hsp(gNXC12H26) = 48.8137006246771 * ( T * ((-2.62181594e+00_WP) + T * ((0.0736188555_WP) + T * ((-3.14656757e-05_WP) + T * ((7.6860317e-09_WP) + T * ((-8.0720446e-13_WP)))))) + (-4.00654253e+04_WP))
    Cpsp(gNXC12H26) = 48.8137006246771 * ((-2.62181594e+00_WP) + T * ((1.47237711e-01_WP) + T * ((-9.43970271e-05_WP) + T * ((3.07441268e-08_WP) + T * (-4.03602230e-12_WP)))))

    hsp(gOC12H23OOH) = 38.4367949998151 * ( T * ((8.80733000e+00_WP) + T * ((0.03253115_WP) + T * ((2.31686e-05_WP) + T * ((-3.172625e-08_WP) + T * ((1.021982e-11_WP)))))) + (-6.65361000e+04_WP))
    Cpsp(gOC12H23OOH) = 38.4367949998151 * ((8.80733000e+00_WP) + T * ((6.50623000e-02_WP) + T * ((6.95058000e-05_WP) + T * ((-1.26905000e-07_WP) + T * (5.10991000e-11_WP)))))

    hsp(gCH2) = 592.780550406388 * ( T * ((3.76267867e+00_WP) + T * ((0.0004844360715_WP) + T * ((9.31632803333333e-07_WP) + T * ((-9.627278825e-10_WP) + T * ((3.37483438e-13_WP)))))) + (4.60040401e+04_WP))
    Cpsp(gCH2) = 592.780550406388 * ((3.76267867e+00_WP) + T * ((9.68872143e-04_WP) + T * ((2.79489841e-06_WP) + T * ((-3.85091153e-09_WP) + T * (1.68741719e-12_WP)))))

    hsp(gHCO) = 286.523537114894 * ( T * ((4.22118584e+00_WP) + T * ((-0.00162196266_WP) + T * ((4.59331486666667e-06_WP) + T * ((-3.328602325e-09_WP) + T * ((8.6753773e-13_WP)))))) + (3.83956496e+03_WP))
    Cpsp(gHCO) = 286.523537114894 * ((4.22118584e+00_WP) + T * ((-3.24392532e-03_WP) + T * ((1.37799446e-05_WP) + T * ((-1.33144093e-08_WP) + T * (4.33768865e-12_WP)))))

    hsp(gCH2D) = 592.780550406388 * ( T * ((4.19860411e+00_WP) + T * ((-0.001183307095_WP) + T * ((2.74432073333333e-06_WP) + T * ((-1.6720399525e-09_WP) + T * ((3.88629474e-13_WP)))))) + (5.04968163e+04_WP))
    Cpsp(gCH2D) = 592.780550406388 * ((4.19860411e+00_WP) + T * ((-2.36661419e-03_WP) + T * ((8.23296220e-06_WP) + T * ((-6.68815981e-09_WP) + T * (1.94314737e-12_WP)))))

    hsp(gCH3O) = 267.910678610556 * ( T * ((3.71180502e+00_WP) + T * ((-0.00140231653_WP) + T * ((1.25516990333333e-05_WP) + T * ((-1.1826802225e-08_WP) + T * ((3.7317684e-12_WP)))))) + (1.29569760e+03_WP))
    Cpsp(gCH3O) = 267.910678610556 * ((3.71180502e+00_WP) + T * ((-2.80463306e-03_WP) + T * ((3.76550971e-05_WP) + T * ((-4.73072089e-08_WP) + T * (1.86588420e-11_WP)))))

    hsp(gC2H3) = 307.437509244195 * ( T * ((3.21246645e+00_WP) + T * ((0.00075739581_WP) + T * ((8.64031373333333e-06_WP) + T * ((-8.941446175e-09_WP) + T * ((2.94301746e-12_WP)))))) + (3.48598468e+04_WP))
    Cpsp(gC2H3) = 307.437509244195 * ((3.21246645e+00_WP) + T * ((1.51479162e-03_WP) + T * ((2.59209412e-05_WP) + T * ((-3.57657847e-08_WP) + T * (1.47150873e-11_WP)))))

    hsp(gCH2CHO) = 193.159093021095 * ( T * ((3.40906240e+00_WP) + T * ((0.005369287_WP) + T * ((6.304975e-07_WP) + T * ((-1.789645775e-09_WP) + T * ((5.7347702e-13_WP)))))) + (6.20000000e+01_WP))
    Cpsp(gCH2CHO) = 193.159093021095 * ((3.40906240e+00_WP) + T * ((1.07385740e-02_WP) + T * ((1.89149250e-06_WP) + T * ((-7.15858310e-09_WP) + T * (2.86738510e-12_WP)))))

    hsp(gC2H5) = 286.109428768066 * ( T * ((4.30646568e+00_WP) + T * ((-0.00209329446_WP) + T * ((1.65714269e-05_WP) + T * ((-1.497816515e-08_WP) + T * ((4.61018008e-12_WP)))))) + (1.28416265e+04_WP))
    Cpsp(gC2H5) = 286.109428768066 * ((4.30646568e+00_WP) + T * ((-4.18658892e-03_WP) + T * ((4.97142807e-05_WP) + T * ((-5.99126606e-08_WP) + T * (2.30509004e-11_WP)))))

    hsp(gAXC3H5) = 202.443145848551 * ( T * ((1.36318350e+00_WP) + T * ((0.0099069105_WP) + T * ((4.16568666666667e-06_WP) + T * ((-8.33888875e-09_WP) + T * ((3.1693142e-12_WP)))))) + (1.92456290e+04_WP))
    Cpsp(gAXC3H5) = 202.443145848551 * ((1.36318350e+00_WP) + T * ((1.98138210e-02_WP) + T * ((1.24970600e-05_WP) + T * ((-3.33555550e-08_WP) + T * (1.58465710e-11_WP)))))

    hsp(gC2H3CHO) = 148.306161035996 * ( T * ((1.27134980e+00_WP) + T * ((0.013115527_WP) + T * ((-3.09707683333333e-06_WP) + T * ((-1.1959318e-09_WP) + T * ((6.6961086e-13_WP)))))) + (-9.33573440e+03_WP))
    Cpsp(gC2H3CHO) = 148.306161035996 * ((1.27134980e+00_WP) + T * ((2.62310540e-02_WP) + T * ((-9.29123050e-06_WP) + T * ((-4.78372720e-09_WP) + T * (3.34805430e-12_WP)))))

    hsp(gNXC3H7) = 192.970802580885 * ( T * ((1.04911730e+00_WP) + T * ((0.0130044865_WP) + T * ((7.84750533333333e-07_WP) + T * ((-4.898783e-09_WP) + T * ((1.87440414e-12_WP)))))) + (1.03123460e+04_WP))
    Cpsp(gNXC3H7) = 192.970802580885 * ((1.04911730e+00_WP) + T * ((2.60089730e-02_WP) + T * ((2.35425160e-06_WP) + T * ((-1.95951320e-08_WP) + T * (9.37202070e-12_WP)))))

    hsp(gC4H7) = 150.906417888776 * ( T * ((7.44494320e-01_WP) + T * ((0.0198394285_WP) + T * ((-7.63269533333333e-06_WP) + T * ((5.33824325e-10_WP) + T * ((4.619275e-13_WP)))))) + (2.26533280e+04_WP))
    Cpsp(gC4H7) = 150.906417888776 * ((7.44494320e-01_WP) + T * ((3.96788570e-02_WP) + T * ((-2.28980860e-05_WP) + T * ((2.13529730e-09_WP) + T * (2.30963750e-12_WP)))))

    hsp(gPXC4H9) = 145.579562964001 * ( T * ((1.20870420e+00_WP) + T * ((0.0191487485_WP) + T * ((-2.42201696666667e-06_WP) + T * ((-3.85713675e-09_WP) + T * ((1.7371887e-12_WP)))))) + (7.32210400e+03_WP))
    Cpsp(gPXC4H9) = 145.579562964001 * ((1.20870420e+00_WP) + T * ((3.82974970e-02_WP) + T * ((-7.26605090e-06_WP) + T * ((-1.54285470e-08_WP) + T * (8.68594350e-12_WP)))))

    hsp(gPXC5H11) = 116.876212432174 * ( T * ((5.24384081e-02_WP) + T * ((0.0280398479_WP) + T * ((-1.10515267666667e-05_WP) + T * ((2.4438344525e-09_WP) + T * ((-2.2801932e-13_WP)))))) + (4.71611460e+03_WP))
    Cpsp(gPXC5H11) = 116.876212432174 * ((5.24384081e-02_WP) + T * ((5.60796958e-02_WP) + T * ((-3.31545803e-05_WP) + T * ((9.77533781e-09_WP) + T * (-1.14009660e-12_WP)))))

    hsp(gPXC7H15) = 83.8223611251134 * ( T * ((-4.99570406e-01_WP) + T * ((0.04044132335_WP) + T * ((-1.66844251333333e-05_WP) + T * ((3.9137327e-09_WP) + T * ((-3.93232454e-13_WP)))))) + (-1.04590223e+03_WP))
    Cpsp(gPXC7H15) = 83.8223611251134 * ((-4.99570406e-01_WP) + T * ((8.08826467e-02_WP) + T * ((-5.00532754e-05_WP) + T * ((1.56549308e-08_WP) + T * (-1.96616227e-12_WP)))))

    hsp(gPXC12H25) = 49.1042995511458 * ( T * ((-1.85028741e+00_WP) + T * ((0.071335354_WP) + T * ((-3.06305518333333e-05_WP) + T * ((7.5220848e-09_WP) + T * ((-7.949086e-13_WP)))))) + (-1.54530435e+04_WP))
    Cpsp(gPXC12H25) = 49.1042995511458 * ((-1.85028741e+00_WP) + T * ((1.42670708e-01_WP) + T * ((-9.18916555e-05_WP) + T * ((3.00883392e-08_WP) + T * (-3.97454300e-12_WP)))))

    hsp(gS3XC12H25) = 49.1042995511458 * ( T * ((-1.36787089e+00_WP) + T * ((0.068677674_WP) + T * ((-2.74692052666667e-05_WP) + T * ((5.91053905e-09_WP) + T * ((-4.94871864e-13_WP)))))) + (-1.67660539e+04_WP))
    Cpsp(gS3XC12H25) = 49.1042995511458 * ((-1.36787089e+00_WP) + T * ((1.37355348e-01_WP) + T * ((-8.24076158e-05_WP) + T * ((2.36421562e-08_WP) + T * (-2.47435932e-12_WP)))))

    hsp(gSXC12H25) = 49.1042995511458 * ( T * ((-1.36787089e+00_WP) + T * ((0.068677674_WP) + T * ((-2.74692052666667e-05_WP) + T * ((5.91053905e-09_WP) + T * ((-4.94871864e-13_WP)))))) + (-1.67660539e+04_WP))
    Cpsp(gSXC12H25) = 49.1042995511458 * ((-1.36787089e+00_WP) + T * ((1.37355348e-01_WP) + T * ((-8.24076158e-05_WP) + T * ((2.36421562e-08_WP) + T * (-2.47435932e-12_WP)))))

    hsp(gC12OOH) = 41.2991257699185 * ( T * ((5.15231000e+00_WP) + T * ((0.04989565_WP) + T * ((-6.02116666666667e-06_WP) + T * ((-1.0460875e-08_WP) + T * ((4.45572e-12_WP)))))) + (-2.38380000e+04_WP))
    Cpsp(gC12OOH) = 41.2991257699185 * ((5.15231000e+00_WP) + T * ((9.97913000e-02_WP) + T * ((-1.80635000e-05_WP) + T * ((-4.18435000e-08_WP) + T * (2.22786000e-11_WP)))))

    hsp(gO2C12H24OOH) = 35.6349219955426 * ( T * ((4.81972000e-01_WP) + T * ((0.07251_WP) + T * ((-3.33102666666667e-05_WP) + T * ((6.51055e-09_WP) + T * ((2.38716e-13_WP)))))) + (-4.16875000e+04_WP))
    Cpsp(gO2C12H24OOH) = 35.6349219955426 * ((4.81972000e-01_WP) + T * ((1.45020000e-01_WP) + T * ((-9.99308000e-05_WP) + T * ((2.60422000e-08_WP) + T * (1.19358000e-12_WP)))))

  end if

  ! Species with specific medium temperature

  return
end subroutine fcmech_thermodata


! ================= !
! Export thermodata !
! ================= !
subroutine fcmech_get_thermodata(h,cp,T)
  use fcmech
  implicit none

  real(WP), dimension(npS) :: h,Cp
  real(WP) :: T
  
  ! Calculate correct values from T
  call fcmech_thermodata(T)

  ! Export h and Cp arrays
  h = hsp
  Cp = Cpsp

  return
end subroutine fcmech_get_thermodata

! ===================== !
! dCp/dT computation !
! ===================== !
subroutine fcmech_compute_dCpsdT(T,dCpsdT)
  use fcmech

  implicit none

  real(WP), dimension(npS), intent(out) :: dCpsdT
  real(WP), intent(in) :: T

  ! All species with default medium temperature of 1000K
  if (T.gt.1000) then

    dCpsdT(gN2) = 296.728765167737 * ((1.48797680e-03_WP) + T*(2.0_WP*(-5.68476000e-07_WP) + T*(3.0_WP*(1.00970380e-10_WP) + T*4.0_WP*(-6.75335100e-15_WP))))
    dCpsdT(gO) = 519.64625 * ((-8.59741137e-05_WP) + T*(2.0_WP*(4.19484589e-08_WP) + T*(3.0_WP*(-1.00177799e-11_WP) + T*4.0_WP*(1.22833691e-15_WP))))
    dCpsdT(gH2) = 4124.17658730159 * ((-4.94024731e-05_WP) + T*(2.0_WP*(4.99456778e-07_WP) + T*(3.0_WP*(-1.79566394e-10_WP) + T*4.0_WP*(2.00255376e-14_WP))))
    dCpsdT(gH) = 8248.35317460317 * ((-2.30842973e-11_WP) + T*(2.0_WP*(1.61561948e-14_WP) + T*(3.0_WP*(-4.73515235e-18_WP) + T*4.0_WP*(4.98197357e-22_WP))))
    dCpsdT(gOH) = 488.848777046096 * ((1.05650448e-03_WP) + T*(2.0_WP*(-2.59082758e-07_WP) + T*(3.0_WP*(3.05218674e-11_WP) + T*4.0_WP*(-1.33195876e-15_WP))))
    dCpsdT(gH2O) = 461.497557726465 * ((2.17691804e-03_WP) + T*(2.0_WP*(-1.64072518e-07_WP) + T*(3.0_WP*(-9.70419870e-11_WP) + T*4.0_WP*(1.68200992e-14_WP))))
    dCpsdT(gH2O2) = 244.424388523048 * ((4.90831694e-03_WP) + T*(2.0_WP*(-1.90139225e-06_WP) + T*(3.0_WP*(3.71185986e-10_WP) + T*4.0_WP*(-2.87908305e-14_WP))))
    dCpsdT(gO2) = 259.823125 * ((1.48308754e-03_WP) + T*(2.0_WP*(-7.57966669e-07_WP) + T*(3.0_WP*(2.09470555e-10_WP) + T*4.0_WP*(-2.16717794e-14_WP))))
    dCpsdT(gHO2) = 251.888633058652 * ((2.23982013e-03_WP) + T*(2.0_WP*(-6.33658150e-07_WP) + T*(3.0_WP*(1.14246370e-10_WP) + T*4.0_WP*(-1.07908535e-14_WP))))
    dCpsdT(gCH2O) = 276.904682608406 * ((9.20000082e-03_WP) + T*(2.0_WP*(-4.42258813e-06_WP) + T*(3.0_WP*(1.00641212e-09_WP) + T*4.0_WP*(-8.83855640e-14_WP))))
    dCpsdT(gCO2) = 188.919336514429 * ((4.41437026e-03_WP) + T*(2.0_WP*(-2.21481404e-06_WP) + T*(3.0_WP*(5.23490188e-10_WP) + T*4.0_WP*(-4.72084164e-14_WP))))
    dCpsdT(gCH3) = 553.035785552747 * ((7.23990037e-03_WP) + T*(2.0_WP*(-2.98714348e-06_WP) + T*(3.0_WP*(5.95684644e-10_WP) + T*4.0_WP*(-4.67154394e-14_WP))))
    dCpsdT(gCO) = 296.834701892181 * ((2.06252743e-03_WP) + T*(2.0_WP*(-9.98825771e-07_WP) + T*(3.0_WP*(2.30053008e-10_WP) + T*4.0_WP*(-2.03647716e-14_WP))))
    dCpsdT(gC2H6) = 276.517892776374 * ((2.16852677e-02_WP) + T*(2.0_WP*(-1.00256067e-05_WP) + T*(3.0_WP*(2.21412001e-09_WP) + T*4.0_WP*(-1.90002890e-13_WP))))
    dCpsdT(gCH4) = 518.285749906495 * ((1.33909467e-02_WP) + T*(2.0_WP*(-5.73285809e-06_WP) + T*(3.0_WP*(1.22292535e-09_WP) + T*4.0_WP*(-1.01815230e-13_WP))))
    dCpsdT(gC2H4) = 296.390275203194 * ((1.46454151e-02_WP) + T*(2.0_WP*(-6.71077915e-06_WP) + T*(3.0_WP*(1.47222923e-09_WP) + T*4.0_WP*(-1.25706061e-13_WP))))
    dCpsdT(gC2H2) = 319.340144415425 * ((5.96166664e-03_WP) + T*(2.0_WP*(-2.37294852e-06_WP) + T*(3.0_WP*(4.67412171e-10_WP) + T*4.0_WP*(-3.61235213e-14_WP))))
    dCpsdT(gC3H6) = 197.593516802129 * ((1.49083400e-02_WP) + T*(2.0_WP*(-4.94989900e-06_WP) + T*(3.0_WP*(7.21202200e-10_WP) + T*4.0_WP*(-3.76620400e-14_WP))))
    dCpsdT(gC4H81) = 148.195137601597 * ((3.43505070e-02_WP) + T*(2.0_WP*(-1.58831970e-05_WP) + T*(3.0_WP*(3.30896620e-09_WP) + T*4.0_WP*(-2.53610450e-13_WP))))
    dCpsdT(gC5H10) = 118.556110081278 * ((4.12429986e-02_WP) + T*(2.0_WP*(-1.84390497e-05_WP) + T*(3.0_WP*(3.06155241e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC6H12) = 98.7967584010647 * ((4.96637321e-02_WP) + T*(2.0_WP*(-2.22947517e-05_WP) + T*(3.0_WP*(3.71602180e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC7H14) = 84.6829357723412 * ((5.80490524e-02_WP) + T*(2.0_WP*(-2.61288124e-05_WP) + T*(3.0_WP*(4.36602746e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC8H16) = 74.0975688007985 * ((6.65524251e-02_WP) + T*(2.0_WP*(-3.00409456e-05_WP) + T*(3.0_WP*(5.03258102e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC9H18) = 65.8645056007098 * ((7.50189757e-02_WP) + T*(2.0_WP*(-3.39279528e-05_WP) + T*(3.0_WP*(5.69367977e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC10H20) = 59.2780550406388 * ((8.34887330e-02_WP) + T*(2.0_WP*(-3.78172786e-05_WP) + T*(3.0_WP*(6.35528704e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC12H25O2) = 41.2991257699185 * ((5.37539000e-02_WP) + T*(2.0_WP*(-1.68186000e-05_WP) + T*(3.0_WP*(2.51367000e-09_WP) + T*4.0_WP*(-1.47208000e-13_WP))))
    dCpsdT(gNXC12H26) = 48.8137006246771 * ((1.07615296e-01_WP) + T*(2.0_WP*(-4.89001251e-05_WP) + T*(3.0_WP*(8.23896743e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gOC12H23OOH) = 38.4367949998151 * ((6.16392000e-02_WP) + T*(2.0_WP*(-2.09836000e-05_WP) + T*(3.0_WP*(3.33166000e-09_WP) + T*4.0_WP*(-2.03590000e-13_WP))))
    dCpsdT(gCH2) = 592.780550406388 * ((3.65639292e-03_WP) + T*(2.0_WP*(-1.40894597e-06_WP) + T*(3.0_WP*(2.60179549e-10_WP) + T*4.0_WP*(-1.87727567e-14_WP))))
    dCpsdT(gHCO) = 286.523537114894 * ((4.95695526e-03_WP) + T*(2.0_WP*(-2.48445613e-06_WP) + T*(3.0_WP*(5.89161778e-10_WP) + T*4.0_WP*(-5.33508711e-14_WP))))
    dCpsdT(gCH2D) = 592.780550406388 * ((4.65588637e-03_WP) + T*(2.0_WP*(-2.01191947e-06_WP) + T*(3.0_WP*(4.17906000e-10_WP) + T*4.0_WP*(-3.39716365e-14_WP))))
    dCpsdT(gCH3O) = 267.910678610556 * ((7.44142474e-03_WP) + T*(2.0_WP*(-2.69705176e-06_WP) + T*(3.0_WP*(4.38090504e-10_WP) + T*4.0_WP*(-2.63537098e-14_WP))))
    dCpsdT(gC2H3) = 307.437509244195 * ((1.03302292e-02_WP) + T*(2.0_WP*(-4.68082349e-06_WP) + T*(3.0_WP*(1.01763288e-09_WP) + T*4.0_WP*(-8.62607041e-14_WP))))
    dCpsdT(gCH2CHO) = 193.159093021095 * ((8.13059140e-03_WP) + T*(2.0_WP*(-2.74362450e-06_WP) + T*(3.0_WP*(4.07030410e-10_WP) + T*4.0_WP*(-2.17601710e-14_WP))))
    dCpsdT(gC2H5) = 286.109428768066 * ((1.73972722e-02_WP) + T*(2.0_WP*(-7.98206668e-06_WP) + T*(3.0_WP*(1.75217689e-09_WP) + T*4.0_WP*(-1.49641576e-13_WP))))
    dCpsdT(gAXC3H5) = 202.443145848551 * ((1.43247310e-02_WP) + T*(2.0_WP*(-5.67816320e-06_WP) + T*(3.0_WP*(1.10808010e-09_WP) + T*4.0_WP*(-9.03638870e-14_WP))))
    dCpsdT(gC2H3CHO) = 148.306161035996 * ((1.71142560e-02_WP) + T*(2.0_WP*(-7.48341610e-06_WP) + T*(3.0_WP*(1.42522490e-09_WP) + T*4.0_WP*(-9.17468410e-14_WP))))
    dCpsdT(gNXC3H7) = 192.970802580885 * ((1.60314850e-02_WP) + T*(2.0_WP*(-5.27202380e-06_WP) + T*(3.0_WP*(7.58883520e-10_WP) + T*4.0_WP*(-3.88627190e-14_WP))))
    dCpsdT(gC4H7) = 150.906417888776 * ((2.26345580e-02_WP) + T*(2.0_WP*(-9.25454700e-06_WP) + T*(3.0_WP*(1.68079270e-09_WP) + T*4.0_WP*(-1.04086170e-13_WP))))
    dCpsdT(gPXC4H9) = 145.579562964001 * ((2.36910710e-02_WP) + T*(2.0_WP*(-7.59488650e-06_WP) + T*(3.0_WP*(6.64271360e-10_WP) + T*4.0_WP*(5.48451360e-14_WP))))
    dCpsdT(gPXC5H11) = 116.876212432174 * ((4.46500156e-02_WP) + T*(2.0_WP*(-2.00797119e-05_WP) + T*(3.0_WP*(3.34852346e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gPXC7H15) = 83.8223611251134 * ((6.14628445e-02_WP) + T*(2.0_WP*(-2.77753660e-05_WP) + T*(3.0_WP*(4.65470891e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gPXC12H25) = 49.1042995511458 * ((1.03781439e-01_WP) + T*(2.0_WP*(-4.72009520e-05_WP) + T*(3.0_WP*(7.95823570e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gS3XC12H25) = 49.1042995511458 * ((1.07348546e-01_WP) + T*(2.0_WP*(-4.96289797e-05_WP) + T*(3.0_WP*(8.49285371e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gSXC12H25) = 49.1042995511458 * ((1.07348546e-01_WP) + T*(2.0_WP*(-4.96289797e-05_WP) + T*(3.0_WP*(8.49285371e-09_WP) + T*4.0_WP*(0.00000000e+00_WP))))
    dCpsdT(gC12OOH) = 41.2991257699185 * ((5.15917000e-02_WP) + T*(2.0_WP*(-1.57327000e-05_WP) + T*(3.0_WP*(2.30306000e-09_WP) + T*4.0_WP*(-1.32640000e-13_WP))))
    dCpsdT(gO2C12H24OOH) = 35.6349219955426 * ((5.10590000e-02_WP) + T*(2.0_WP*(-1.54345000e-05_WP) + T*(3.0_WP*(2.24627000e-09_WP) + T*4.0_WP*(-1.28901000e-13_WP))))

  else 

    dCpsdT(gN2) = 296.728765167737 * ((1.40824040e-03_WP) + T*(2.0_WP*(-3.96322200e-06_WP) + T*(3.0_WP*(5.64151500e-09_WP) + T*4.0_WP*(-2.44485400e-12_WP))))
    dCpsdT(gO) = 519.64625 * ((-3.27931884e-03_WP) + T*(2.0_WP*(6.64306396e-06_WP) + T*(3.0_WP*(-6.12806624e-09_WP) + T*4.0_WP*(2.11265971e-12_WP))))
    dCpsdT(gH2) = 4124.17658730159 * ((7.98052075e-03_WP) + T*(2.0_WP*(-1.94781510e-05_WP) + T*(3.0_WP*(2.01572094e-08_WP) + T*4.0_WP*(-7.37611761e-12_WP))))
    dCpsdT(gH) = 8248.35317460317 * ((7.05332819e-13_WP) + T*(2.0_WP*(-1.99591964e-15_WP) + T*(3.0_WP*(2.30081632e-18_WP) + T*4.0_WP*(-9.27732332e-22_WP))))
    dCpsdT(gOH) = 488.848777046096 * ((-3.22544939e-03_WP) + T*(2.0_WP*(6.52764691e-06_WP) + T*(3.0_WP*(-5.79853643e-09_WP) + T*4.0_WP*(2.06237379e-12_WP))))
    dCpsdT(gH2O) = 461.497557726465 * ((-2.03643410e-03_WP) + T*(2.0_WP*(6.52040211e-06_WP) + T*(3.0_WP*(-5.48797062e-09_WP) + T*4.0_WP*(1.77197817e-12_WP))))
    dCpsdT(gH2O2) = 244.424388523048 * ((-5.42822417e-04_WP) + T*(2.0_WP*(1.67335701e-05_WP) + T*(3.0_WP*(-2.15770813e-08_WP) + T*4.0_WP*(8.62454363e-12_WP))))
    dCpsdT(gO2) = 259.823125 * ((-2.99673416e-03_WP) + T*(2.0_WP*(9.84730201e-06_WP) + T*(3.0_WP*(-9.68129509e-09_WP) + T*4.0_WP*(3.24372837e-12_WP))))
    dCpsdT(gHO2) = 251.888633058652 * ((-4.74912051e-03_WP) + T*(2.0_WP*(2.11582891e-05_WP) + T*(3.0_WP*(-2.42763894e-08_WP) + T*4.0_WP*(9.29225124e-12_WP))))
    dCpsdT(gCH2O) = 276.904682608406 * ((-9.90833369e-03_WP) + T*(2.0_WP*(3.73220008e-05_WP) + T*(3.0_WP*(-3.79285261e-08_WP) + T*4.0_WP*(1.31772652e-11_WP))))
    dCpsdT(gCO2) = 188.919336514429 * ((8.98459677e-03_WP) + T*(2.0_WP*(-7.12356269e-06_WP) + T*(3.0_WP*(2.45919022e-09_WP) + T*4.0_WP*(-1.43699548e-13_WP))))
    dCpsdT(gCH3) = 553.035785552747 * ((2.01095175e-03_WP) + T*(2.0_WP*(5.73021856e-06_WP) + T*(3.0_WP*(-6.87117425e-09_WP) + T*4.0_WP*(2.54385734e-12_WP))))
    dCpsdT(gCO) = 296.834701892181 * ((-6.10353680e-04_WP) + T*(2.0_WP*(1.01681433e-06_WP) + T*(3.0_WP*(9.07005884e-10_WP) + T*4.0_WP*(-9.04424499e-13_WP))))
    dCpsdT(gC2H6) = 276.517892776374 * ((-5.50154270e-03_WP) + T*(2.0_WP*(5.99438288e-05_WP) + T*(3.0_WP*(-7.08466285e-08_WP) + T*4.0_WP*(2.68685771e-11_WP))))
    dCpsdT(gCH4) = 518.285749906495 * ((-1.36709788e-02_WP) + T*(2.0_WP*(4.91800599e-05_WP) + T*(3.0_WP*(-4.84743026e-08_WP) + T*4.0_WP*(1.66693956e-11_WP))))
    dCpsdT(gC2H4) = 296.390275203194 * ((-7.57052247e-03_WP) + T*(2.0_WP*(5.70990292e-05_WP) + T*(3.0_WP*(-6.91588753e-08_WP) + T*4.0_WP*(2.69884373e-11_WP))))
    dCpsdT(gC2H2) = 319.340144415425 * ((2.33615629e-02_WP) + T*(2.0_WP*(-3.55171815e-05_WP) + T*(3.0_WP*(2.80152437e-08_WP) + T*4.0_WP*(-8.50072974e-12_WP))))
    dCpsdT(gC3H6) = 197.593516802129 * ((2.09251800e-02_WP) + T*(2.0_WP*(4.48679400e-06_WP) + T*(3.0_WP*(-1.66891200e-08_WP) + T*4.0_WP*(7.15814600e-12_WP))))
    dCpsdT(gC4H81) = 148.195137601597 * ((3.08533800e-02_WP) + T*(2.0_WP*(5.08652470e-06_WP) + T*(3.0_WP*(-2.46548880e-08_WP) + T*4.0_WP*(1.11101930e-11_WP))))
    dCpsdT(gC5H10) = 118.556110081278 * ((5.74218294e-02_WP) + T*(2.0_WP*(-3.74486890e-05_WP) + T*(3.0_WP*(1.27364989e-08_WP) + T*4.0_WP*(-1.79609789e-12_WP))))
    dCpsdT(gC6H12) = 98.7967584010647 * ((6.98655426e-02_WP) + T*(2.0_WP*(-4.59408022e-05_WP) + T*(3.0_WP*(1.56967343e-08_WP) + T*4.0_WP*(-2.21296175e-12_WP))))
    dCpsdT(gC7H14) = 84.6829357723412 * ((8.24611601e-02_WP) + T*(2.0_WP*(-5.46504108e-05_WP) + T*(3.0_WP*(1.87862303e-08_WP) + T*4.0_WP*(-2.65737983e-12_WP))))
    dCpsdT(gC8H16) = 74.0975688007985 * ((9.46066357e-02_WP) + T*(2.0_WP*(-6.27385521e-05_WP) + T*(3.0_WP*(2.15158309e-08_WP) + T*4.0_WP*(-3.02718683e-12_WP))))
    dCpsdT(gC9H18) = 65.8645056007098 * ((1.06958297e-01_WP) + T*(2.0_WP*(-7.10973244e-05_WP) + T*(3.0_WP*(2.43971077e-08_WP) + T*4.0_WP*(-3.42771547e-12_WP))))
    dCpsdT(gC10H20) = 59.2780550406388 * ((1.19305598e-01_WP) + T*(2.0_WP*(-7.94489025e-05_WP) + T*(3.0_WP*(2.72736596e-08_WP) + T*4.0_WP*(-3.82718373e-12_WP))))
    dCpsdT(gC12H25O2) = 41.2991257699185 * ((8.93873000e-02_WP) + T*(2.0_WP*(1.45351000e-05_WP) + T*(3.0_WP*(-7.49250000e-08_WP) + T*4.0_WP*(3.35325000e-11_WP))))
    dCpsdT(gNXC12H26) = 48.8137006246771 * ((1.47237711e-01_WP) + T*(2.0_WP*(-9.43970271e-05_WP) + T*(3.0_WP*(3.07441268e-08_WP) + T*4.0_WP*(-4.03602230e-12_WP))))
    dCpsdT(gOC12H23OOH) = 38.4367949998151 * ((6.50623000e-02_WP) + T*(2.0_WP*(6.95058000e-05_WP) + T*(3.0_WP*(-1.26905000e-07_WP) + T*4.0_WP*(5.10991000e-11_WP))))
    dCpsdT(gCH2) = 592.780550406388 * ((9.68872143e-04_WP) + T*(2.0_WP*(2.79489841e-06_WP) + T*(3.0_WP*(-3.85091153e-09_WP) + T*4.0_WP*(1.68741719e-12_WP))))
    dCpsdT(gHCO) = 286.523537114894 * ((-3.24392532e-03_WP) + T*(2.0_WP*(1.37799446e-05_WP) + T*(3.0_WP*(-1.33144093e-08_WP) + T*4.0_WP*(4.33768865e-12_WP))))
    dCpsdT(gCH2D) = 592.780550406388 * ((-2.36661419e-03_WP) + T*(2.0_WP*(8.23296220e-06_WP) + T*(3.0_WP*(-6.68815981e-09_WP) + T*4.0_WP*(1.94314737e-12_WP))))
    dCpsdT(gCH3O) = 267.910678610556 * ((-2.80463306e-03_WP) + T*(2.0_WP*(3.76550971e-05_WP) + T*(3.0_WP*(-4.73072089e-08_WP) + T*4.0_WP*(1.86588420e-11_WP))))
    dCpsdT(gC2H3) = 307.437509244195 * ((1.51479162e-03_WP) + T*(2.0_WP*(2.59209412e-05_WP) + T*(3.0_WP*(-3.57657847e-08_WP) + T*4.0_WP*(1.47150873e-11_WP))))
    dCpsdT(gCH2CHO) = 193.159093021095 * ((1.07385740e-02_WP) + T*(2.0_WP*(1.89149250e-06_WP) + T*(3.0_WP*(-7.15858310e-09_WP) + T*4.0_WP*(2.86738510e-12_WP))))
    dCpsdT(gC2H5) = 286.109428768066 * ((-4.18658892e-03_WP) + T*(2.0_WP*(4.97142807e-05_WP) + T*(3.0_WP*(-5.99126606e-08_WP) + T*4.0_WP*(2.30509004e-11_WP))))
    dCpsdT(gAXC3H5) = 202.443145848551 * ((1.98138210e-02_WP) + T*(2.0_WP*(1.24970600e-05_WP) + T*(3.0_WP*(-3.33555550e-08_WP) + T*4.0_WP*(1.58465710e-11_WP))))
    dCpsdT(gC2H3CHO) = 148.306161035996 * ((2.62310540e-02_WP) + T*(2.0_WP*(-9.29123050e-06_WP) + T*(3.0_WP*(-4.78372720e-09_WP) + T*4.0_WP*(3.34805430e-12_WP))))
    dCpsdT(gNXC3H7) = 192.970802580885 * ((2.60089730e-02_WP) + T*(2.0_WP*(2.35425160e-06_WP) + T*(3.0_WP*(-1.95951320e-08_WP) + T*4.0_WP*(9.37202070e-12_WP))))
    dCpsdT(gC4H7) = 150.906417888776 * ((3.96788570e-02_WP) + T*(2.0_WP*(-2.28980860e-05_WP) + T*(3.0_WP*(2.13529730e-09_WP) + T*4.0_WP*(2.30963750e-12_WP))))
    dCpsdT(gPXC4H9) = 145.579562964001 * ((3.82974970e-02_WP) + T*(2.0_WP*(-7.26605090e-06_WP) + T*(3.0_WP*(-1.54285470e-08_WP) + T*4.0_WP*(8.68594350e-12_WP))))
    dCpsdT(gPXC5H11) = 116.876212432174 * ((5.60796958e-02_WP) + T*(2.0_WP*(-3.31545803e-05_WP) + T*(3.0_WP*(9.77533781e-09_WP) + T*4.0_WP*(-1.14009660e-12_WP))))
    dCpsdT(gPXC7H15) = 83.8223611251134 * ((8.08826467e-02_WP) + T*(2.0_WP*(-5.00532754e-05_WP) + T*(3.0_WP*(1.56549308e-08_WP) + T*4.0_WP*(-1.96616227e-12_WP))))
    dCpsdT(gPXC12H25) = 49.1042995511458 * ((1.42670708e-01_WP) + T*(2.0_WP*(-9.18916555e-05_WP) + T*(3.0_WP*(3.00883392e-08_WP) + T*4.0_WP*(-3.97454300e-12_WP))))
    dCpsdT(gS3XC12H25) = 49.1042995511458 * ((1.37355348e-01_WP) + T*(2.0_WP*(-8.24076158e-05_WP) + T*(3.0_WP*(2.36421562e-08_WP) + T*4.0_WP*(-2.47435932e-12_WP))))
    dCpsdT(gSXC12H25) = 49.1042995511458 * ((1.37355348e-01_WP) + T*(2.0_WP*(-8.24076158e-05_WP) + T*(3.0_WP*(2.36421562e-08_WP) + T*4.0_WP*(-2.47435932e-12_WP))))
    dCpsdT(gC12OOH) = 41.2991257699185 * ((9.97913000e-02_WP) + T*(2.0_WP*(-1.80635000e-05_WP) + T*(3.0_WP*(-4.18435000e-08_WP) + T*4.0_WP*(2.22786000e-11_WP))))
    dCpsdT(gO2C12H24OOH) = 35.6349219955426 * ((1.45020000e-01_WP) + T*(2.0_WP*(-9.99308000e-05_WP) + T*(3.0_WP*(2.60422000e-08_WP) + T*4.0_WP*(1.19358000e-12_WP))))
  end if

  ! Species with specific medium temperature

  return
end subroutine fcmech_compute_dCpsdT

! ======================== !
! Pure component viscosity !
! ======================== !
subroutine fcmech_get_viscosity( mu, T )
  use fcmech
  implicit none

  ! Viscosity array 
  real(WP), dimension(npS) :: mu
  ! Temperature 
  real(WP) :: T
  integer :: i

  do i=1,npS
    mu(i) = mucoeff(i)*sqrt(T)/fcmech_omegamu(T*koveps(i))
  end do

  return
end subroutine fcmech_get_viscosity

! =========================== !
! Pure component conductivity !
! =========================== !
subroutine fcmech_get_conductivity( lambda, T, mu )
  use fcmech
  implicit none

  ! Conductivity array
  real(WP), dimension(npS) :: lambda
  ! Temperature
  real(WP) :: T
  ! Viscosity and co.
  real(WP), dimension(npS) :: mu

  lambda = mu*(Cpsp+1.2_WP*R_cst/Wsp)

  return
end subroutine fcmech_get_conductivity

! ======================================== !
! Inverse of binary diffusion coefficients !
! ======================================== !
subroutine fcmech_get_invDij( invDij, T, P )
  use fcmech
  implicit none

  ! Diffusion coefficients array 
  real(WP), dimension(npS,npS) :: invDij
  ! Temperature 
  real(WP) :: T
  ! Pressure 
  real(WP) :: P
  ! Mix 
  real(WP) :: TPterm
  integer :: i,j

  ! Pressure and temperature dependent term
  TPterm = P/(T*sqrt(T))
  do i=1,npS
    do j=1,i-1
      invDij(i,j) = TPterm*fcmech_omegaD(T*Ocoeffs(i,j))/Dcoeffs(i,j)
      invDij(j,i) = invDij(i,j)
    end do
    invDij(i,i) = 0.0_WP
  end do

  return
end subroutine fcmech_get_invDij

