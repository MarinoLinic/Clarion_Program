

   MEMBER('ZavrsniML.clw')                                 ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE

                     MAP
                       INCLUDE('ZAVRSNIML003.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
IspisOdredeneNarudzbe PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(NARUCITELJ)
                       PROJECT(NARU:Adresa_narucitelja)
                       PROJECT(NARU:Ime_i_prezime_narucitelja)
                       PROJECT(NARU:OIB_narucitelja)
                       PROJECT(NARU:Postanski_broj)
                       PROJECT(NARU:Telefon_narucitelja)
                       JOIN(NAR:VK_NarudzbenicaOIB,NARU:OIB_narucitelja)
                         PROJECT(NAR:Broj_narudzbe)
                         PROJECT(NAR:Datum)
                         PROJECT(NAR:Napomena)
                         PROJECT(NAR:PDV)
                         PROJECT(NAR:Sveukupno)
                         PROJECT(NAR:Sifra_izdavacke_kuce)
                         PROJECT(NAR:Sifra_nacina_dostave)
                         JOIN(IZD:PK_IzdavackaKuca,NAR:Sifra_izdavacke_kuce)
                           PROJECT(IZD:Adresa_izdavacke_kuce)
                           PROJECT(IZD:Email_izdavacke_kuce)
                           PROJECT(IZD:IBAN_izdavacke_kuce)
                           PROJECT(IZD:Naziv_izdavacke_kuce)
                           PROJECT(IZD:Postanski_broj)
                           PROJECT(IZD:Telefon_izdavacke_kuce)
                         END
                         JOIN(NAC:PK_NacinDostave,NAR:Sifra_nacina_dostave)
                           PROJECT(NAC:Naziv_nacina_dostave)
                         END
                         JOIN(STA:PK_Stavka,NAR:Broj_narudzbe)
                           PROJECT(STA:Iznos_stavke)
                           PROJECT(STA:Kolicina_stavke)
                           PROJECT(STA:Sifra_artikla)
                           JOIN(ART:PK_Artikl,STA:Sifra_artikla)
                             PROJECT(ART:Naziv_artikla)
                             PROJECT(ART:Sifra_artikla)
                           END
                         END
                       END
                       JOIN(MJE:PK_Mjesto,NARU:Postanski_broj)
                         PROJECT(MJE:Naziv_mjesta)
                       END
                     END
ReportPageNumber     LONG,AUTO
ProgressWindow       WINDOW('Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2000,6250,7688),PRE(RPT),PAPER(PAPER:A4),FONT('Cambria',10,,FONT:regular,CHARSET:ANSI), |
  THOUS
                       HEADER,AT(1000,1000,6250,1042),USE(?Header)
                         IMAGE('Backup\CroatiaBooksTextwh.jpg'),AT(42,-10,917,979),USE(?IMAGE1)
                         STRING('Datum izvjesca:'),AT(3792,115),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(4937,115),USE(?ReportDateStamp),TRN
                         STRING('Vrijeme izvjesca:'),AT(3792,365),USE(?ReportTimePrompt),TRN
                         STRING('<<-- Time Stamp -->'),AT(4937,365),USE(?ReportTimeStamp),TRN
                       END
StankaNar              BREAK(NAR:Broj_narudzbe),USE(?BREAK2)
                         HEADER,AT(0,0,6250,4521),USE(?GROUPHEADER1)
                           STRING(@s254),AT(562,240,5208,281),USE(NAR:Napomena),FONT('Arial',10),CENTER
                           STRING('Narudzba broj:'),AT(469,583,1344,271),USE(?STRING3),FONT(,15)
                           STRING(@P#########P),AT(1875,583,1385,271),USE(NAR:Broj_narudzbe,,?NAR:Broj_narudzbe:2),FONT(, |
  15)
                           LINE,AT(448,1187,4260,0),USE(?LINE2),LINEWIDTH(1)
                           STRING('Podaci za naplatu i dostavu:'),AT(469,1260,1917,187),USE(?STRING2:2),FONT(,10,,FONT:bold)
                           STRING('Nacin dostave:'),AT(3177,1260,1031,187),USE(?STRING2:3),FONT(,10,,FONT:bold)
                           STRING(@s49),AT(469,1510,2229,187),USE(NARU:Ime_i_prezime_narucitelja,,?NARU:Ime_i_prezime_narucitelja:2)
                           STRING(@s49),AT(469,1760,2229,187),USE(NARU:Adresa_narucitelja,,?NARU:Adresa_narucitelja:2)
                           STRING(@P###/###-###P),AT(656,2260,1042,187),USE(NARU:Telefon_narucitelja,,?NARU:Telefon_narucitelja:2)
                           STRING(@s5),AT(469,2010,490,187),USE(NARU:Postanski_broj,,?NARU:Postanski_broj:2)
                           STRING('T:'),AT(469,2260,125,187),USE(?STRING4)
                           STRING(@s49),AT(3177,1510,1479,187),USE(NAC:Naziv_nacina_dostave,,?NAC:Naziv_nacina_dostave:2)
                           LINE,AT(448,2833,4260,0),USE(?LINE2:2),LINEWIDTH(1)
                           STRING('IBAN:'),AT(469,2906,646,187),USE(?STRING4:2),FONT(,11,,FONT:bold)
                           STRING('E-mail:'),AT(469,3156,646,187),USE(?STRING4:3),FONT(,11,,FONT:bold)
                           STRING('Telefon:'),AT(469,3406,646,187),USE(?STRING4:4),FONT(,11,,FONT:bold)
                           STRING('Datum:'),AT(3177,3156,646,187),USE(?STRING4:5),FONT(,11,,FONT:bold)
                           STRING(@d17),AT(3812,3156,1073,208),USE(NAR:Datum,,?NAR:Datum:2),FONT(,11)
                           STRING(@s21),AT(1177,2906,1937,208),USE(IZD:IBAN_izdavacke_kuce,,?IZD:IBAN_izdavacke_kuce:2), |
  FONT(,11)
                           STRING(@P###/###-###P),AT(1177,3406,1146,208),USE(IZD:Telefon_izdavacke_kuce,,?IZD:Telefon_izdavacke_kuce:2), |
  FONT(,11)
                           STRING(@s49),AT(1177,3156,1771,208),USE(IZD:Email_izdavacke_kuce,,?IZD:Email_izdavacke_kuce:2), |
  FONT(,11)
                           LINE,AT(469,4229,5375,0),USE(?LINE2:3),LINEWIDTH(3)
                           STRING('Artikl'),AT(625,4323,490,187),USE(?STRING2:4),FONT(,10,,FONT:bold)
                           STRING('Sifra'),AT(3500,4323,427,187),USE(?STRING2:5),FONT(,10,,FONT:bold)
                           STRING('Kol.'),AT(4292,4323,427,187),USE(?STRING2:6),FONT(,10,,FONT:bold)
                           STRING('Ukupno'),AT(5042,4323,542,187),USE(?STRING2:7),FONT(,10,,FONT:bold)
                         END
Detail                   DETAIL,AT(0,0,6250,333),USE(?Detail)
                           LINE,AT(500,42,5260,0),USE(?LINE2:7),COLOR(00D3D3D3h),LINEWIDTH(1)
                           STRING(@n-7),AT(3906,135,604,187),USE(STA:Kolicina_stavke),CENTER
                           STRING(@n-14.2),AT(4510,135,885,187),USE(STA:Iznos_stavke),RIGHT
                           STRING('kn'),AT(5458,135,156,187),USE(?STRING5:2)
                           STRING(@s49),AT(625,115,1833),USE(ART:Naziv_artikla),FONT(,,,FONT:bold)
                           STRING(@n-7),AT(3177,135),USE(ART:Sifra_artikla),CENTER
                         END
                         FOOTER,AT(0,0,6250,1292),USE(?GROUPFOOTER1)
                           STRING(@n-14.2),AT(5042,490,792),USE(NAR:PDV),LEFT
                           STRING(@n-14.2),AT(5042,229,812),USE(NAR:Sveukupno),LEFT
                           STRING('Sveukupno:'),AT(4167,229),USE(?STRING6)
                           STRING('PDV:'),AT(4167,490,510,198),USE(?STRING6:2)
                         END
                       END
                       FOOTER,AT(1000,9688,6250,1000),USE(?Footer)
                         IMAGE('Backup\CroatiaBookswhth.jpg'),AT(5240,31,958,646),USE(?IMAGE1:2)
                         STRING('Str:'),AT(5573,729),USE(?STRING1)
                         STRING(@s49),AT(1250,292,2719),USE(IZD:Naziv_izdavacke_kuce,,?IZD:Naziv_izdavacke_kuce:2), |
  FONT(,,,FONT:bold)
                         STRING(@s49),AT(208,542,3510),USE(IZD:Adresa_izdavacke_kuce,,?IZD:Adresa_izdavacke_kuce:2), |
  FONT(,,,FONT:bold)
                         STRING(@s5),AT(208,729,469),USE(IZD:Postanski_broj),FONT(,,,FONT:bold)
                         STRING(@s49),AT(698,729,3021),USE(MJE:Naziv_mjesta,,?MJE:Naziv_mjesta:2),FONT(,,,FONT:bold)
                         LINE,AT(229,219,4344,0),USE(?LINE1),COLOR(0000008Bh),LINEWIDTH(2)
                         STRING('Izdavacka kuca'),AT(208,292),USE(?STRING2),FONT(,,,FONT:bold)
                         STRING(@N3),AT(5771,729),USE(ReportPageNumber)
                       END
                       FORM,AT(1000,1000,6250,9688),USE(?Form)
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
OpenReport             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('IspisOdredeneNarudzbe')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:NARUCITELJ.SetOpenRelated()
  Relate:NARUCITELJ.Open                                   ! File NARUCITELJ used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisOdredeneNarudzbe',ProgressWindow)     ! Restore window settings from non-volatile store
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:NARUCITELJ, ?Progress:PctText, Progress:Thermometer, ProgressMgr, NARU:OIB_narucitelja)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(NARU:PK_Narucitelj)
  ThisReport.AddRange(NARU:OIB_narucitelja)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:NARUCITELJ.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:NARUCITELJ.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisOdredeneNarudzbe',ProgressWindow)  ! Save window data to non-volatile store
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OpenReport PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.OpenReport()
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportDateStamp{PROP:Text} = FORMAT(TODAY(),@D17)
  END
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportTimeStamp{PROP:Text} = FORMAT(CLOCK(),@T7)
  END
  IF ReturnValue = Level:Benign
    Report$?ReportPageNumber{PROP:PageNo} = True
  END
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:Detail)
  RETURN ReturnValue

