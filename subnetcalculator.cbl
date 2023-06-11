       IDENTIFICATION DIVISION. 
       PROGRAM-ID. SUBNETCALCULATOR.
       AUTHOR. EMMA SKOVGAARD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
          01 WorkingIP PIC X(15) VALUE "0.0.0.0".
          01 IPClassPortion PIC 9(3) VALUE ZEROES.
          01 DesiredClients PIC 9(8) VALUE ZEROES.
          01 TotalClients PIC 9(8) VALUE 2.
          01 TotalClientsOutput PIC Z(8) VALUE 2.
          01 UsableClients PIC 9(32) VALUE 0.
          01 UsableClientsOutput PIC Z(8) VALUE 0.
          01 DesiredSubnets PIC 9(8) VALUE ZEROES.
          01 CurrentSubnets PIC 9(7) VALUE 256.
          01 CurrentSubnetsOutput PIC Z(7) VALUE ZEROES.
          01 CurrentSubnets2 PIC 9(7) VALUE 2.
          01 TotalBits PIC 9(2) VALUE ZEROES. 
          01 TotalBitsOutput PIC Z(2) VALUE ZEROES. 
          01 CurrentBits PIC 9(2) VALUE 1. 
          01 NetworkClass PIC A(1) VALUE "X".
          01 CustomSubnet PIC 9(3) VALUE 0.
          01 CustomSubnet2 PIC 9(3) VALUE 256.
          01 SubnetValue PIC 9(3) VALUE 256.
          01 SubnetBinary PIC 9(3) VALUE 2.
          01 SubnetBinary2 PIC 9(3) VALUE 2.
          01 SubnetAmountBinary PIC 9(3) VALUE 128.
          01 SubnetAmountBinary2 PIC 9(3) VALUE 128.
          01 MaxBits PIC 9(2) VALUE 8.
       PROCEDURE DIVISION.
         ACCEPTINFO.
           DISPLAY "Welcome to Emma's Custom COBOL Subnet Calculator."
           DISPLAY "Type in the working IP: " WITH NO ADVANCING 
           ACCEPT WorkingIP.
           DISPLAY "Type in the amount of desired usable clients: " 
      -    WITH NO ADVANCING.
           ACCEPT DesiredClients.
           DISPLAY "Type in the amount of desired subnets: "
      -    WITH NO ADVANCING.
           ACCEPT DesiredSubnets
           MOVE WorkingIP TO IPClassPortion.
           IF IPClassPortion < 128 THEN
              MOVE "A" TO NetworkClass 
              DISPLAY "Class A, default subnet mask: 255.0.0.0"
              COMPUTE MaxBits = 24
              IF DesiredClients > 16777214 THEN
                 DISPLAY 
      -          "ERROR: This network cannot handle that many hosts"
                 STOP RUN
              END-IF
           ELSE IF IPClassPortion < 192 THEN
              MOVE "B" TO NetworkClass 
              DISPLAY "Class B, default subnet mask: 255.255.0.0"
              COMPUTE MaxBits = 16
              IF DesiredClients > 65534 THEN
                 DISPLAY 
      -          "ERROR: This network cannot handle that many hosts"
                 STOP RUN
              END-IF
           ELSE IF IPClassPortion < 224 THEN
              MOVE "C" TO NetworkClass 
              DISPLAY "Class C, default subnet mask: 255.255.255.0"
              COMPUTE MaxBits = 8
              IF DesiredClients > 254 THEN
                 DISPLAY 
      -          "ERROR: This network cannot handle that many hosts"
                 STOP RUN
              END-IF
           ELSE IF IPClassPortion < 239 THEN
              MOVE "D" TO NetworkClass 
              DISPLAY "Class D, default subnet mask: 255.255.255.0"
              COMPUTE MaxBits = 8
              IF DesiredClients > 15 THEN
                 DISPLAY 
      -          "ERROR: This network cannot handle that many hosts"
                 STOP RUN
              END-IF
           ELSE IF IPClassPortion < 255 THEN
              MOVE "E" TO NetworkClass 
              DISPLAY "Class E, default subnet mask: 255.255.255.0"
              COMPUTE MaxBits = 8
              IF DesiredClients > 15 THEN
                 DISPLAY 
      -          "ERROR: This network cannot handle that many hosts"
                 STOP RUN
              END-IF
           END-IF.
           PERFORM UNTIL CurrentSubnets > DesiredSubnets 
              COMPUTE SubnetBinary2 = SubnetBinary2 /2
              COMPUTE CustomSubnet2 = SubnetValue  - SubnetBinary2
              COMPUTE CurrentSubnets2 = CurrentSubnets2 * 2
              IF CurrentSubnets > DesiredSubnets THEN
                 DISPLAY
      -          "//SUBNET PRIORITY SECTION//"       
                  IF IPClassPortion < 128 THEN
                    DISPLAY "Custom subnet: 255.", CustomSubnet2,".0.0"
                  ELSE IF IPClassPortion < 192 THEN
                    DISPLAY "Custom subnet: 255.255", CustomSubnet2,".0"
                  ELSE IF IPClassPortion < 256 THEN
                    DISPLAY "Custom subnet: 255.255.255.", CustomSubnet2
                  END-IF
              END-IF
           END-PERFORM
           PERFORM UNTIL TotalClients > DesiredClients
               COMPUTE CurrentBits = CurrentBits + 1
               COMPUTE TotalClients = TotalClients * 2
               COMPUTE UsableClients = TotalClients - 2 
               COMPUTE SubnetBinary = SubnetBinary * 2
               IF SubnetBinary >= 256 THEN
                  COMPUTE SubnetBinary = 1
               END-IF
               IF UsableClients >= DesiredClients THEN
                  DISPLAY
      -           "//HOST PRIORITY SECTION//"
                  PERFORM CurrentBits TIMES 
                    COMPUTE CurrentSubnets = CurrentSubnets /2
                  END-PERFORM
                  MOVE CurrentSubnets TO CurrentSubnetsOutput 
                  MOVE TotalClients TO TotalClientsOutput 
                  DISPLAY "Total clients: ", TotalClientsOutput
                  MOVE UsableClients TO UsableClientsOutput  
                  DISPLAY "Usable clients: ", UsableClientsOutput
                  COMPUTE TotalBits = MaxBits - CurrentBits
                  MOVE TotalBits TO TotalBitsOutput 
                  DISPLAY "Bits: ", TotalBitsOutput
                  COMPUTE CustomSubnet = SubnetValue - SubnetBinary 
                  DISPLAY "Subnets: ", CurrentSubnetsOutput
                  IF IPClassPortion < 128 THEN
                    DISPLAY "Custom subnet: 255.", CustomSubnet,".0.0"
                  ELSE IF IPClassPortion < 192 THEN
                    DISPLAY "Custom subnet: 255.255", CustomSubnet,".0"
                  ELSE IF IPClassPortion < 256 THEN
                    DISPLAY "Custom subnet: 255.255.255.", CustomSubnet
                  END-IF
               END-IF
           END-PERFORM.

           STOP RUN.