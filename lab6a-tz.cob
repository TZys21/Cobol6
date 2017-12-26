        *> Tyler Zysberg
        *> A program to create a break report with subtotals
            
            
        Identification Division.
   
        Program-Id.        lab6.  
        
        environment division.
        input-output section.
        file-control.
           select inFile assign to "lab6a-in.dat"
               organization is line sequential.
         
           select outFile assign to "lab6a-out.dat"
               organization is line sequential.
        
        
        Data Division.
        
        File Section.
        FD inFile.
          01  in-record.
           05 in-RegionNum              PIC X(2).
           05 in-RegionName              PIC X(15).
           05 in-DepartmentNumber          PIC X(5).
           05 in-DptName               Pic X(30).
           05 in-EmpNumber        Pic X(5).
           05 in-LastName               PIC X(20).
           05 in-FirstName               PIC X(15).
           05 in-Gender                 Pic X(1).
           05 in-Adress                  PIC X(20).
           05 in-CityState              PIC X(20).
           05 in-JobTitle               PIC X(20).
           05 in-DOB                    Pic 9(8).
           05 in-DOH                    PIC 9(8).
           05 in-MaritalStatus                pic X(1).
           05 in-Deps             PIC 9(2).
           05 in-SchoolDistrict.
             10 SchoolDistrict1      pic 9.
             10 SchoolDistrict2      pic 9.
             10 SchoolDistrict3      pic 9.
           05 in-medcovered                  PIC X.
           05 in-dentalcovered                  PIC X.
           05 in-visioncovered                  PIC X.
           05 in-401k                    PIC v9(3).
           05 in-PayCode                pic X.
           05 in-Pay                pic S9(7)v99.
           05 in-HoursPerweek         Pic S9(2)v99.
           05 in-commissionrate                 PIC v999.
           05 in-SalesAmount            PIC S9(7)v99.
        
            
        FD outFile.
        01    out-rec pic X(250).
        
        Working-Storage Section.
        
        01  eof                    pic x value "n".
    
        01  rpt-fields.
            05  page-no            pic 9(3)  value 0.
            05  lines-per-page     pic 9(2)  value 35.
            05  line-no            pic 9(2)  value 40.
        
        01  ws-date.
            05  ws-yr              pic 9(4).
            05  ws-mo              pic 99.
            05  ws-dy              pic 99.
        01 ws-time.
            05 ws-hr pic 99.
            05 ws-min pic 99.
            05 ws-sec pic 99.

        01  total-fields. 
            05  subtotal    pic 9(11)v99 value zero.
            05  total        pic 9(12)v99 value zero.
            
        01  control-fields.
            05               pic x(12) value "Department: ".
            05 dptnum        pic x(5) value low-values.
            
        01 DptName      pic x(30).
        01  pg-hdr.
            05 ph-Month PIC Z9/.
            05 ph-Day   PIC 99/.
            05 ph-Year  PIC 9999.
            05          PIC X(50) Value Spaces.
            05         PIC X(27) Value "Stomper & Wombat's Emporium".
            05          PIC X(50) Value Spaces.
			05          PIC X(5) value "Page:".
            05 ph-Page  PIC Z9.
        
        01 pg-hdr2. *> Variables for displaying second line of header
            05          PIC X Value Spaces.
            05 ph-hr    PIC Z9.
            05          PIC X Value ":".
            05 ph-mn    PIC 99.
            05          PIC X Value Spaces.
            05 ph-ampm  PIC XX.
            05          PIC X(50) Value Spaces.
            05          PIC X(29) Value "Monthly Gross Payroll Listing".      
        
        01  col-hdr.
            05                     pic x(17) value "Emp #".
            05                     pic x(22) value "Employee".
            05                     pic x(3) value "M".
            05                     pic x(6) value "Deps".
            05                     pic x(8)  value "Ins".
            05                     pic x(15) value "Gross Pay".
            05                     pic x(17) value "Commission".
            05                     pic x(12) value "401K".
            05                     pic x(11) value "Fed".
            05                     pic x(10)  value "State".
            05                     pic x(16) value "Insurance".
            05                     pic x(8)  value "Net Pay".            
        
        01  dtl-line.
            05                      pic X.
            05  dl-Empnum           pic x(5).
            05                     pic x(12) value spaces.
            05  dl-Last            pic x(10).
            05                     pic x value spaces.
            05  dl-FirstInitial     pic x. 
            05                     pic x(9) value spaces.
            05  dl-Marital       pic x.
            05                     pic x(3) value spaces.
            05  dl-Deps          pic 99.
            05                     pic x(3) value spaces.
            05  dl-Ins             pic x(3).
            05                     pic x value spaces.
            05  dl-Gross-Pay       pic $$$,$$$,$$9.99.
            05                     pic x value spaces.
            05  dl-Commission      pic $Z,ZZZ,ZZZ.ZZ.
            05                     pic x(3) value spaces.
            05  dl-401K            pic $$$,$$9.99.
            05                     pic x(3) value spaces.
            05  dl-fed             pic $$$,$$9.99.
            05                     pic x(3) value spaces.
            05 dl-state            pic $$$,$$9.99.
            05                     pic x(3) value spaces.
            05 dl-insurance        pic $$$,$$9.99.
            05                     pic x(3) value spaces.
            05 dl-NetPay           pic $$$,$$9.99.

        
        01  dpt-subtotal-line.
            05                     pic x(100) value spaces.
            05                     pic x(5)   value "Dept ".
            05  tl-dpt-no          pic x(5).
            05                     pic x value spaces.
            05             pic x(22) value "Total for department: ".
            05  tl-total-dept    pic $$$,$$$,$$$,$$9.99.
            

        01  total-line.
            05                     pic x(100) value spaces.
            05                     pic x(14) value "Total Payroll:".
            05  tl-total-payroll     pic $$$,$$$,$$$,$$9.99.
           
        
        01 indx             pic 9 value 0.
        01 401Kpercent   pic 99V99. 
        01 Ws-Gross-Pay  pic 9(7)V99.
        01 Ws-Commission pic 9(7)V99.
        01 Ws-totalSal   pic 9(7)V99.
        01 Ws-401K       pic 9(5)V99.
        01 Ws-Fed        pic 9(5)V99.
        01 Ws-State      pic 9(5)V99.
        01 Ws-Insurance  pic 9(5)V99.
        01 Ws-NetPay     pic 9(11)V99.
        01 editCommis    pic Z(7).ZZ.
        01 insuranceChars. 
            05 med   PIC X.
            05 dental   PIC X.
            05 vision   PIC X.
        01  blank-line             pic x value spaces.
		01 counter pic 9 value 0.
        
        
        Procedure Division.
        000-main.
                  
         Perform 100-initialize
         perform 200-getDate
         Perform until eof = "Y"
            read inFile
            at end move "Y" to eof
                
            not at end
              
              perform 210-getRec
              perform 500-Finalize
              perform 600-ReportPrint
              
            end-read
            End-Perform
            
            move total to tl-total-payroll
             
             if line-no > lines-per-page 
               perform 400-new-page
           end-if.
           
           write out-rec from total-line after advancing 1 line
           
           close inFile.
           close outFile.
         
            stop run.
        100-initialize.
       
           open input inFile.
           open output outFile.
           

            
           
        120-ControlBreak.   
           
           move in-DepartmentNumber to dptnum
            move in-DptName to DptName
            if line-no >= lines-per-page 
               perform 400-new-page
           end-if
           write out-rec from control-fields after advancing 1 line
           add 1 to line-no
          
           write out-rec from DptName after advancing 1 line
           add 1 to line-no
            if line-no >= lines-per-page
                perform 400-new-page
            end-if
           write out-rec from blank-line after advancing 1 line
            add 1 to line-no.
           
           
           
           
        200-getDate.   
           accept ws-date from date yyyymmdd
           move ws-yr to ph-Year
           move ws-mo to ph-Month
           move ws-dy to ph-Day
           accept ws-time From Time
           
           
           
           IF ws-hr > 12
               subtract 12 from ws-hr
               move "PM" to ph-ampm
               move ws-hr to ph-hr 
           ELSE
              move ws-hr to ph-hr
              move "AM" to ph-ampm
           end-if
           move ws-min to ph-mn.
       
        210-getRec.
            move in-EmpNumber to dl-Empnum
            move in-LastName to dl-Last
            move in-FirstName (1:1) to dl-FirstInitial
            move in-MaritalStatus to dl-Marital
            move in-Deps to dl-deps
            If in-MedCovered = "Y"
                Move "M" to med
            Else
                Move " " to med
            End-If
            If in-DentalCovered = "Y"
                Move "D" to dental
            Else
                Move " " to dental
            End-If
            If in-VisionCovered = "Y"
                Move "V" to vision
            Else
                Move " " to vision
            End-If
                move insuranceChars to dl-Ins

            Perform 220-calculatePay.
            

            
            
        

        220-calculatePay.
            if in-PayCode = "S"
                Divide in-Pay by 12 giving in-Pay rounded
				Move in-Pay to Ws-Gross-Pay
                Move 0 to Ws-Commission
			end-if
                
            If in-PayCode = "H"
                Compute in-Pay rounded = 52 * in-Pay / 12 * 
                in-HoursPerWeek
                Move in-Pay to Ws-Gross-Pay
                Move 0 to Ws-Commission

            End-If
            If in-PayCode = "C"
                Compute in-Pay rounded = in-Pay / 12
                Move in-Pay to Ws-Gross-Pay
                compute Ws-Commission Rounded 
                        = in-SalesAmount * in-CommissionRate               


            End-If
            Perform 300-calculateDeduc.


        300-calculateDeduc.    
            Compute 401Kpercent = in-401k * 100
            Compute Ws-totalSal rounded = Ws-Gross-Pay + Ws-Commission 
            Compute Ws-401K rounded = Ws-totalSal * in-401k.
            if in-MaritalStatus = "M" or "P"
                Compute Ws-Fed rounded = (Ws-totalSal - Ws-401K) * 0.28 
            Else
                Compute Ws-Fed rounded  = (Ws-totalSal - Ws-401K) * 0.33
            End-if.
            Compute Ws-State rounded = (Ws-totalSal - Ws-401K - Ws-Fed) 
                            * 0.0475
                            
            move 0 to Ws-Insurance                
            if in-Deps >= 2  
                if med = "M"
                    compute Ws-Insurance = Ws-Insurance + 100
                    
                End-If    
                If dental = "D"
                    compute Ws-Insurance = Ws-Insurance + 40
                End-If
                If vision = "V"
                    compute Ws-Insurance = Ws-Insurance + 7.5

                End-if
            Else
                if med = "M"
                    compute Ws-Insurance = Ws-Insurance + 75
                End-if
                if dental = "D"
                    compute Ws-Insurance = Ws-Insurance + 25
                End-If
                if vision = "V"
                    compute Ws-Insurance = Ws-Insurance + 5
                End-If
            End-If
			
            Compute Ws-NetPay rounded = Ws-totalSal - Ws-401K - Ws-Fed 
                    - Ws-State - Ws-Insurance
			
            
            Compute subtotal rounded = subtotal + Ws-NetPay

            Compute total rounded = total + subtotal.
                     
        400-new-page.
           move 0 to line-no
           if page-no > 0
               write out-rec from blank-line
                   after advancing 1 line
                   add 1 to line-no
           end-if
           
           add 1 to page-no
           move page-no to ph-page
           write out-rec from pg-hdr
               after advancing 1 line
               add 1 to line-no
           write out-rec from pg-hdr2
               after advancing 1 line
                add 1 to line-no
           write out-rec from blank-line after advancing 1 line
                add 1 to line-no
           write out-rec from blank-line after advancing 1 line
              add 1 to line-no.        
                            
        500-Finalize.
            move Ws-Gross-Pay to dl-Gross-Pay
            if Ws-Commission = 0
                move editCommis to dl-Commission
            else    
                move Ws-Commission to dl-Commission
            end-if    
            move Ws-401K to dl-401K
            move Ws-Fed to dl-Fed
            move Ws-State to dl-state
            move Ws-Insurance to dl-insurance
            move Ws-NetPay to dl-NetPay.
              
        600-ReportPrint.
           if line-no > lines-per-page 
               perform 400-new-page
           end-if.
          
          
		   
		   if in-DepartmentNumber <> dptnum
              if dptnum = low-values 
                continue
              else  
                
				move dptnum to tl-dpt-no
                if counter = 0
                   compute subtotal rounded = subtotal - Ws-NetPay
                   compute counter = counter + 1
				end-if
                move subtotal to tl-total-dept
				move 0 to subtotal
                if line-no > lines-per-page 
                    perform 400-new-page
                end-if   
                    write out-rec from dpt-subtotal-line after 
                            advancing 1 line
                    add 1 to line-no
					
              end-if
              
                perform 120-ControlBreak
              
                if line-no > lines-per-page 
                    perform 400-new-page
                end-if
                
                write out-rec from col-hdr after advancing 1 line
                add 1 to line-no
                write out-rec from blank-line after advancing 1 line
                add 1 to line-no
            End-if 
			
			
			
            if line-no > lines-per-page 
               perform 400-new-page
           end-if

           write out-rec from dtl-line after advancing 1 line
           add 1 to line-no.              
              