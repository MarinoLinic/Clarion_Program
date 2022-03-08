

   MEMBER('ZavrsniML.clw')                                 ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('ZAVRSNIML002.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('ZAVRSNIML001.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeMjesta PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::MJE:Record  LIKE(MJE:RECORD),THREAD
FormWindow           WINDOW('Azuriranje mjesta...'),AT(,,228,90),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@s5),AT(61,16,60,10),USE(MJE:Postanski_broj),REQ,TIP('Potrebno je unijeti 5-ero z' & |
  'namenkasti postanski broj.')
                       ENTRY(@s49),AT(61,39,60,10),USE(MJE:Naziv_mjesta),CAP
                       BUTTON('OK'),AT(8,64,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(54,64,40,12),USE(?Cancel)
                       STRING(@S40),AT(97,66),USE(ActionMessage)
                       PROMPT('Postanski broj:'),AT(11,18),USE(?MJE:Postanski_broj:Prompt)
                       PROMPT('Naziv mjesta:'),AT(11,40),USE(?MJE:Naziv_mjesta:Prompt)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Podatak ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Podatak ce biti izmijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeMjesta')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MJE:Postanski_broj
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(MJE:Record,History::MJE:Record)
  SELF.AddHistoryField(?MJE:Postanski_broj,1)
  SELF.AddHistoryField(?MJE:Naziv_mjesta,2)
  SELF.AddUpdateFile(Access:MJESTO)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:MJESTO.SetOpenRelated()
  Relate:MJESTO.Open                                       ! File MJESTO used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:MJESTO
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeMjesta',FormWindow)              ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:MJESTO.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeMjesta',FormWindow)           ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeJM PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::JED:Record  LIKE(JED:RECORD),THREAD
FormWindow           WINDOW('Azuriraj jedinice mjere...'),AT(,,222,154),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@n-7),AT(74,16,60,10),USE(JED:Sifra_jedinice_mjere,,?JED:Sifra_jedinice_mjere:2),RIGHT(1), |
  REQ
                       SPIN(@s19),AT(74,36,60,10),USE(JED:Naziv_jedinice_mjere),FROM('HRK|#kn|USD|#$|EUR|#€|YE' & |
  'N|#yen|DKK|#kr')
                       BUTTON('OK'),AT(4,58,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(50,58,40,12),USE(?Cancel)
                       STRING(@S40),AT(92,60),USE(ActionMessage)
                       PROMPT('Naziv jedinice mjere:'),AT(5,35),USE(?JED:Naziv_jedinice_mjere:Prompt)
                       PROMPT('Sifra jedinice mjere:'),AT(5,16),USE(?JED:Sifra_jedinice_mjere:Prompt:2)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeJM')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?JED:Sifra_jedinice_mjere:2
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(JED:Record,History::JED:Record)
  SELF.AddHistoryField(?JED:Sifra_jedinice_mjere:2,1)
  SELF.AddHistoryField(?JED:Naziv_jedinice_mjere,2)
  SELF.AddUpdateFile(Access:JEDINICA_MJERE)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:JEDINICA_MJERE.SetOpenRelated()
  Relate:JEDINICA_MJERE.Open                               ! File JEDINICA_MJERE used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:JEDINICA_MJERE
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeJM',FormWindow)                  ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:JEDINICA_MJERE.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeJM',FormWindow)               ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeIK PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::IZD:Record  LIKE(IZD:RECORD),THREAD
FormWindow           WINDOW('Azuriraj izdavacku kucu...'),AT(,,289,159),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@P#####P),AT(82,9,60,10),USE(IZD:Sifra_izdavacke_kuce),TIP('Sifra IK ima 5 brojev' & |
  'nih znakova')
                       ENTRY(@s49),AT(82,29,60,10),USE(IZD:Naziv_izdavacke_kuce),CAP
                       ENTRY(@s49),AT(82,49,60,10),USE(IZD:Adresa_izdavacke_kuce),CAP
                       ENTRY(@s49),AT(82,68,60,10),USE(IZD:Email_izdavacke_kuce)
                       ENTRY(@P###/###-###P),AT(82,86,60,10),USE(IZD:Telefon_izdavacke_kuce),TIP('Telefonski b' & |
  'roj ima 10 znakova')
                       ENTRY(@s21),AT(82,106,60,10),USE(IZD:IBAN_izdavacke_kuce),TIP('IBAN ima 2 slova oznacav' & |
  'ajuci drzavu i 19 brojeva')
                       ENTRY(@s5),AT(82,126,60,10),USE(IZD:Postanski_broj),TIP('Potrebno je unijeti 5-ero znam' & |
  'enkasti postanski broj.')
                       BUTTON('OK'),AT(5,140,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Cancel'),AT(50,140,40,12),USE(?Cancel)
                       STRING(@S40),AT(93,142),USE(ActionMessage)
                       PROMPT('Sifra izdavacke kuce:'),AT(5,10),USE(?IZD:Sifra_izdavacke_kuce:Prompt)
                       PROMPT('Naziv izdavacke kuce:'),AT(5,30),USE(?IZD:Naziv_izdavacke_kuce:Prompt)
                       PROMPT('Adresa izdavacke kuce:'),AT(5,50),USE(?IZD:Adresa_izdavacke_kuce:Prompt)
                       PROMPT('Email izdavacke kuce:'),AT(5,68),USE(?IZD:Email_izdavacke_kuce:Prompt)
                       PROMPT('Telefon izdavacke kuce:'),AT(4,87),USE(?IZD:Telefon_izdavacke_kuce:Prompt)
                       PROMPT('IBAN izdavacke kuce:'),AT(3,106),USE(?IZD:IBAN_izdavacke_kuce:Prompt)
                       PROMPT('Postanski broj:'),AT(5,126),USE(?IZD:Postanski_broj:Prompt)
                       STRING(@s49),AT(150,126),USE(MJE:Naziv_mjesta)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeIK')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?IZD:Sifra_izdavacke_kuce
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(IZD:Record,History::IZD:Record)
  SELF.AddHistoryField(?IZD:Sifra_izdavacke_kuce,1)
  SELF.AddHistoryField(?IZD:Naziv_izdavacke_kuce,2)
  SELF.AddHistoryField(?IZD:Adresa_izdavacke_kuce,3)
  SELF.AddHistoryField(?IZD:Email_izdavacke_kuce,4)
  SELF.AddHistoryField(?IZD:Telefon_izdavacke_kuce,5)
  SELF.AddHistoryField(?IZD:IBAN_izdavacke_kuce,6)
  SELF.AddHistoryField(?IZD:Postanski_broj,7)
  SELF.AddUpdateFile(Access:IZDAVACKA_KUCA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:IZDAVACKA_KUCA.SetOpenRelated()
  Relate:IZDAVACKA_KUCA.Open                               ! File IZDAVACKA_KUCA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:IZDAVACKA_KUCA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeIK',FormWindow)                  ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:IZDAVACKA_KUCA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeIK',FormWindow)               ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  MJE:Postanski_broj = IZD:Postanski_broj                  ! Assign linking field value
  Access:MJESTO.Fetch(MJE:PK_Mjesto)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PregledMjesta
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?IZD:Postanski_broj
      MJE:Postanski_broj = IZD:Postanski_broj
      IF Access:MJESTO.TryFetch(MJE:PK_Mjesto)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          IZD:Postanski_broj = MJE:Postanski_broj
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeNarucitelja PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::NARU:Record LIKE(NARU:RECORD),THREAD
FormWindow           WINDOW('Azuriranje narucitelja...'),AT(,,289,159),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@s11),AT(96,13,60,10),USE(NARU:OIB_narucitelja),TIP('11-znamenkasti broj, OIB')
                       ENTRY(@s49),AT(96,26,60,10),USE(NARU:Ime_i_prezime_narucitelja),CAP
                       ENTRY(@s49),AT(96,40,60,10),USE(NARU:Adresa_narucitelja),CAP
                       ENTRY(@P###/###-###P),AT(96,54,60,10),USE(NARU:Telefon_narucitelja),TIP('Telefonski bro' & |
  'j ima 10 karakternih znakova')
                       ENTRY(@s5),AT(96,76,60,10),USE(NARU:Postanski_broj),TIP('Potrebno je unijeti 5-ero znam' & |
  'enkasti postanski broj.')
                       BUTTON('OK'),AT(5,100,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(50,100,40,12),USE(?Cancel)
                       STRING(@S40),AT(95,100),USE(ActionMessage)
                       PROMPT('OIB narucitelja:'),AT(5,14),USE(?NARU:OIB_narucitelj:Prompt)
                       PROMPT('Ime i prezime narucitelja:'),AT(5,26),USE(?NARU:Ime_i_prezime_narucitelja:Prompt)
                       PROMPT('Adresa narucitelja:'),AT(5,40),USE(?NARU:Adresa_narucitelja:Prompt)
                       PROMPT('Telefon narucitelja:'),AT(4,56),USE(?NARU:Telefon_narucitelja:Prompt)
                       PROMPT('Postanski broj:'),AT(5,76),USE(?NARU:Postanski_broj:Prompt)
                       STRING(@s49),AT(170,76),USE(MJE:Naziv_mjesta)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeNarucitelja')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?NARU:OIB_narucitelja
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(NARU:Record,History::NARU:Record)
  SELF.AddHistoryField(?NARU:OIB_narucitelja,1)
  SELF.AddHistoryField(?NARU:Ime_i_prezime_narucitelja,2)
  SELF.AddHistoryField(?NARU:Adresa_narucitelja,3)
  SELF.AddHistoryField(?NARU:Telefon_narucitelja,4)
  SELF.AddHistoryField(?NARU:Postanski_broj,5)
  SELF.AddUpdateFile(Access:NARUCITELJ)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:NARUCITELJ.SetOpenRelated()
  Relate:NARUCITELJ.Open                                   ! File NARUCITELJ used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:NARUCITELJ
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeNarucitelja',FormWindow)         ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
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
    INIMgr.Update('AzuriranjeNarucitelja',FormWindow)      ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  MJE:Postanski_broj = NARU:Postanski_broj                 ! Assign linking field value
  Access:MJESTO.Fetch(MJE:PK_Mjesto)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PregledMjesta
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?NARU:Postanski_broj
      MJE:Postanski_broj = NARU:Postanski_broj
      IF Access:MJESTO.TryFetch(MJE:PK_Mjesto)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          NARU:Postanski_broj = MJE:Postanski_broj
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeND PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::NAC:Record  LIKE(NAC:RECORD),THREAD
FormWindow           WINDOW('Azuriraj nacin dostave...'),AT(,,289,159),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@P#####P),AT(115,31,60,10),USE(NAC:Sifra_nacina_dostave),REQ
                       OPTION('Naziv nacina dostave:'),AT(12,57,100),USE(NAC:Naziv_nacina_dostave),BOXED
                         RADIO('STANDARDNO'),AT(114,57),USE(?NAC:Naziv_nacina_dostave:Radio1),VALUE('Standardna_dostava')
                         RADIO('PREMIUM'),AT(114,70),USE(?NAC:Naziv_nacina_dostave:Radio2),VALUE('Premium_dostava')
                         RADIO('BRZO'),AT(114,83),USE(?NAC:Naziv_nacina_dostave:Radio3),VALUE('Brza_dostava')
                         RADIO('BESPLATNO'),AT(114,96),USE(?NAC:Naziv_nacina_dostave:Radio4),VALUE('Besplatna_dostava')
                       END
                       BUTTON('OK'),AT(5,140,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(50,140,40,12),USE(?Cancel)
                       STRING(@S40),AT(95,140),USE(ActionMessage)
                       PROMPT('Sifra nacina dostave:'),AT(12,30),USE(?NAC:Sifra_nacina_dostave:Prompt)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeND')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?NAC:Sifra_nacina_dostave
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(NAC:Record,History::NAC:Record)
  SELF.AddHistoryField(?NAC:Sifra_nacina_dostave,1)
  SELF.AddHistoryField(?NAC:Naziv_nacina_dostave,2)
  SELF.AddUpdateFile(Access:NACIN_DOSTAVE)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:NACIN_DOSTAVE.Open                                ! File NACIN_DOSTAVE used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:NACIN_DOSTAVE
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeND',FormWindow)                  ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:NACIN_DOSTAVE.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeND',FormWindow)               ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeNarudzbenice PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
BRW5::View:Browse    VIEW(STAVKA)
                       PROJECT(STA:Redni_broj_stavke)
                       PROJECT(STA:Broj_narudzbe)
                       PROJECT(STA:Sifra_artikla)
                       PROJECT(STA:Kolicina_stavke)
                       PROJECT(STA:Iznos_stavke)
                       JOIN(ART:PK_Artikl,STA:Sifra_artikla)
                         PROJECT(ART:Naziv_artikla)
                         PROJECT(ART:Sifra_artikla)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
STA:Redni_broj_stavke  LIKE(STA:Redni_broj_stavke)    !List box control field - type derived from field
STA:Broj_narudzbe      LIKE(STA:Broj_narudzbe)        !List box control field - type derived from field
STA:Sifra_artikla      LIKE(STA:Sifra_artikla)        !List box control field - type derived from field
ART:Naziv_artikla      LIKE(ART:Naziv_artikla)        !List box control field - type derived from field
STA:Kolicina_stavke    LIKE(STA:Kolicina_stavke)      !List box control field - type derived from field
STA:Iznos_stavke       LIKE(STA:Iznos_stavke)         !List box control field - type derived from field
ART:Sifra_artikla      LIKE(ART:Sifra_artikla)        !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::NAR:Record  LIKE(NAR:RECORD),THREAD
FormWindow           WINDOW('Azuriraj narudzbenicu...'),AT(,,508,248),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@P#########P),AT(84,6,60,10),USE(NAR:Broj_narudzbe),TIP('Broj narudzbe ima 9 brojeva')
                       ENTRY(@d17),AT(244,6,60,10),USE(NAR:Datum),TIP('DD/MM/YYYY')
                       ENTRY(@s11),AT(434,6,60,10),USE(NAR:OIB_narucitelja),TIP('11-znamenkasti broj, OIB')
                       ENTRY(@P#####P),AT(84,30,60,10),USE(NAR:Sifra_izdavacke_kuce,,?NAR:Sifra_izdavacke_kuce:2), |
  TIP('Sifra IK ima 5 brojevnih znakova')
                       ENTRY(@P#####P),AT(244,30,60,10),USE(NAR:Sifra_nacina_dostave,,?NAR:Sifra_nacina_dostave:2)
                       BUTTON('&Unesi'),AT(364,74,42,12),USE(?Insert)
                       BUTTON('Izracunaj PDV'),AT(169,180),USE(?BUTTON1)
                       ENTRY(@n-14.2),AT(84,183,60,10),USE(NAR:PDV),DECIMAL(12)
                       ENTRY(@s254),AT(66,210,214,31),USE(NAR:Napomena)
                       BUTTON('OK'),AT(284,229,40,12),USE(?OK),DEFAULT,REQ
                       ENTRY(@n-14.2),AT(84,158,60,10),USE(NAR:Sveukupno),DECIMAL(12)
                       BUTTON('Prekini'),AT(364,229,40,12),USE(?Cancel)
                       BUTTON('&Promijeni'),AT(364,98,42,12),USE(?Change)
                       BUTTON('&Izbrisi'),AT(364,122,42,12),USE(?Delete)
                       STRING(@S40),AT(284,209),USE(ActionMessage)
                       PROMPT('Broj narudzbe:'),AT(4,7),USE(?NAR:Broj_narudzbe:Prompt)
                       PROMPT('Napomena:'),AT(6,220),USE(?NAR:Napomena:Prompt)
                       PROMPT('Datum:'),AT(176,7),USE(?NAR:Datum:Prompt)
                       PROMPT('PDV:'),AT(6,183),USE(?NAR:PDV:Prompt)
                       PROMPT('Sveukupno:'),AT(4,158),USE(?NAR:Sveukupno:Prompt)
                       PROMPT('OIB narucitelja:'),AT(354,6),USE(?NARU:OIB_narucitelj:Prompt:2)
                       PROMPT('Sifra izdavacke kuce:'),AT(4,30),USE(?NAR:Sifra_izdavacke_kuce:Prompt:2)
                       PROMPT('Sifra nacina dostave:'),AT(164,31),USE(?NAR:Sifra_nacina_dostave:Prompt:2)
                       STRING(@s49),AT(434,20,61),USE(NARU:Ime_i_prezime_narucitelja)
                       STRING(@s49),AT(84,43,61),USE(IZD:Naziv_izdavacke_kuce)
                       STRING(@s49),AT(244,44,61),USE(NAC:Naziv_nacina_dostave)
                       LIST,AT(4,66,356,82),USE(?List),RIGHT(1),HVSCROLL,CURSOR(CURSOR:IBeam),FORMAT('60C|M~Redn' & |
  'i broj stavke~@n-14@44C|M~Br. narudzbe~@P#{9}P@[42C|M~Sifra artikla~@n-7@81C|M~Naziv' & |
  ' artikla~@s49@]|~Artikal~55C|M~Kolicina stavke~@n-7@60C|M~Iznos stavke~@n-14.2@'),FROM(Queue:Browse), |
  IMM
                       STRING(@n-14.2),AT(244,183),USE(NAR:PDV,,?NAR:PDV:2)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
BRW5                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetFromView          PROCEDURE(),DERIVED
                     END

BRW5::Sort0:Locator  StepLocatorClass                      ! Default Locator
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeNarudzbenice')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?NAR:Broj_narudzbe
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(NAR:Record,History::NAR:Record)
  SELF.AddHistoryField(?NAR:Broj_narudzbe,1)
  SELF.AddHistoryField(?NAR:Datum,3)
  SELF.AddHistoryField(?NAR:OIB_narucitelja,6)
  SELF.AddHistoryField(?NAR:Sifra_izdavacke_kuce:2,7)
  SELF.AddHistoryField(?NAR:Sifra_nacina_dostave:2,8)
  SELF.AddHistoryField(?NAR:PDV,4)
  SELF.AddHistoryField(?NAR:Napomena,2)
  SELF.AddHistoryField(?NAR:Sveukupno,5)
  SELF.AddHistoryField(?NAR:PDV:2,4)
  SELF.AddUpdateFile(Access:NARUDZBENICA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:IZDAVACKA_KUCA.SetOpenRelated()
  Relate:IZDAVACKA_KUCA.Open                               ! File IZDAVACKA_KUCA used by this procedure, so make sure it's RelationManager is open
  Access:NACIN_DOSTAVE.UseFile                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:NARUCITELJ.UseFile                                ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  SELF.Primary &= Relate:NARUDZBENICA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW5.Init(?List,Queue:Browse.ViewPosition,BRW5::View:Browse,Queue:Browse,Relate:STAVKA,SELF) ! Initialize the browse manager
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  BRW5.Q &= Queue:Browse
  BRW5.AddSortOrder(,STA:PK_Stavka)                        ! Add the sort order for STA:PK_Stavka for sort order 1
  BRW5.AddRange(STA:Broj_narudzbe,Relate:STAVKA,Relate:NARUDZBENICA) ! Add file relationship range limit for sort order 1
  BRW5.AddLocator(BRW5::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW5::Sort0:Locator.Init(,STA:Redni_broj_stavke,1,BRW5)  ! Initialize the browse locator using  using key: STA:PK_Stavka , STA:Redni_broj_stavke
  BRW5.AddField(STA:Redni_broj_stavke,BRW5.Q.STA:Redni_broj_stavke) ! Field STA:Redni_broj_stavke is a hot field or requires assignment from browse
  BRW5.AddField(STA:Broj_narudzbe,BRW5.Q.STA:Broj_narudzbe) ! Field STA:Broj_narudzbe is a hot field or requires assignment from browse
  BRW5.AddField(STA:Sifra_artikla,BRW5.Q.STA:Sifra_artikla) ! Field STA:Sifra_artikla is a hot field or requires assignment from browse
  BRW5.AddField(ART:Naziv_artikla,BRW5.Q.ART:Naziv_artikla) ! Field ART:Naziv_artikla is a hot field or requires assignment from browse
  BRW5.AddField(STA:Kolicina_stavke,BRW5.Q.STA:Kolicina_stavke) ! Field STA:Kolicina_stavke is a hot field or requires assignment from browse
  BRW5.AddField(STA:Iznos_stavke,BRW5.Q.STA:Iznos_stavke)  ! Field STA:Iznos_stavke is a hot field or requires assignment from browse
  BRW5.AddField(ART:Sifra_artikla,BRW5.Q.ART:Sifra_artikla) ! Field ART:Sifra_artikla is a hot field or requires assignment from browse
  INIMgr.Fetch('AzuriranjeNarudzbenice',FormWindow)        ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  BRW5.AskProcedure = 4                                    ! Will call: AzuriranjeStavke
  BRW5.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:IZDAVACKA_KUCA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeNarudzbenice',FormWindow)     ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      PregledNarucitelja
      PregledIK
      PregledND
      AzuriranjeStavke
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?BUTTON1
      NAR:PDV=NAR:Sveukupno*0.25
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?NAR:OIB_narucitelja
      NARU:OIB_narucitelja = NAR:OIB_narucitelja
      IF Access:NARUCITELJ.TryFetch(NARU:PK_Narucitelj)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          NAR:OIB_narucitelja = NARU:OIB_narucitelja
        END
      END
      ThisWindow.Reset()
    OF ?NAR:Sifra_izdavacke_kuce:2
      IZD:Sifra_izdavacke_kuce = NAR:Sifra_izdavacke_kuce
      IF Access:IZDAVACKA_KUCA.TryFetch(IZD:PK_IzdavackaKuca)
        IF SELF.Run(2,SelectRecord) = RequestCompleted
          NAR:Sifra_izdavacke_kuce = IZD:Sifra_izdavacke_kuce
        END
      END
      ThisWindow.Reset()
    OF ?NAR:Sifra_nacina_dostave:2
      NAC:Sifra_nacina_dostave = NAR:Sifra_nacina_dostave
      IF Access:NACIN_DOSTAVE.TryFetch(NAC:PK_NacinDostave)
        IF SELF.Run(3,SelectRecord) = RequestCompleted
          NAR:Sifra_nacina_dostave = NAC:Sifra_nacina_dostave
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW5.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW5.ResetFromView PROCEDURE

NAR:Sveukupno:Sum    REAL                                  ! Sum variable for browse totals
  CODE
  SETCURSOR(Cursor:Wait)
  Relate:STAVKA.SetQuickScan(1)
  SELF.Reset
  IF SELF.UseMRP
     IF SELF.View{PROP:IPRequestCount} = 0
          SELF.View{PROP:IPRequestCount} = 60
     END
  END
  LOOP
    IF SELF.UseMRP
       IF SELF.View{PROP:IPRequestCount} = 0
            SELF.View{PROP:IPRequestCount} = 60
       END
    END
    CASE SELF.Next()
    OF Level:Notify
      BREAK
    OF Level:Fatal
      SETCURSOR()
      RETURN
    END
    SELF.SetQueueRecord
    NAR:Sveukupno:Sum += STA:Iznos_stavke
  END
  SELF.View{PROP:IPRequestCount} = 0
  NAR:Sveukupno = NAR:Sveukupno:Sum
  PARENT.ResetFromView
  Relate:STAVKA.SetQuickScan(0)
  SETCURSOR()

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeArtikala PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::ART:Record  LIKE(ART:RECORD),THREAD
FormWindow           WINDOW('Azuriraj artikal...'),AT(,,289,111),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@n-7),AT(78,10,60,10),USE(ART:Sifra_artikla),RIGHT(1)
                       ENTRY(@s49),AT(78,35,60,10),USE(ART:Naziv_artikla)
                       ENTRY(@n-7),AT(78,59,60,10),USE(ART:Sifra_jedinice_mjere,,?ART:Sifra_jedinice_mjere:2),RIGHT(1)
                       BUTTON('OK'),AT(8,84,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(54,84,40,12),USE(?Cancel)
                       STRING(@S40),AT(98,84),USE(ActionMessage)
                       PROMPT('Sifra artikla:'),AT(10,11),USE(?ART:Sifra_artikla:Prompt)
                       PROMPT('Naziv artikla:'),AT(10,34),USE(?ART:Naziv_artikla:Prompt)
                       PROMPT('Sifra jedinice mjere:'),AT(10,58),USE(?ART:Sifra_jedinice_mjere:Prompt:2)
                       STRING(@s19),AT(152,60),USE(JED:Naziv_jedinice_mjere)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeArtikala')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?ART:Sifra_artikla
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(ART:Record,History::ART:Record)
  SELF.AddHistoryField(?ART:Sifra_artikla,1)
  SELF.AddHistoryField(?ART:Naziv_artikla,2)
  SELF.AddHistoryField(?ART:Sifra_jedinice_mjere:2,3)
  SELF.AddUpdateFile(Access:ARTIKL)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:ARTIKL.SetOpenRelated()
  Relate:ARTIKL.Open                                       ! File ARTIKL used by this procedure, so make sure it's RelationManager is open
  Access:JEDINICA_MJERE.UseFile                            ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  SELF.Primary &= Relate:ARTIKL
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeArtikala',FormWindow)            ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:ARTIKL.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeArtikala',FormWindow)         ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PregledJM
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?ART:Sifra_jedinice_mjere:2
      JED:Sifra_jedinice_mjere = ART:Sifra_jedinice_mjere
      IF Access:JEDINICA_MJERE.TryFetch(JED:PK_JedinicaMjere)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          ART:Sifra_jedinice_mjere = JED:Sifra_jedinice_mjere
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeStavke PROCEDURE 

ActionMessage        CSTRING(40)                           ! 
History::STA:Record  LIKE(STA:RECORD),THREAD
FormWindow           WINDOW('Azuriraj stavku...'),AT(,,276,127),CENTER,GRAY,MDI,SYSTEM
                       ENTRY(@n-14),AT(70,10,60,10),USE(STA:Redni_broj_stavke),RIGHT(1)
                       ENTRY(@n-7),AT(70,28,60,10),USE(STA:Sifra_artikla),RIGHT(1)
                       ENTRY(@n-7),AT(70,52,60,10),USE(STA:Kolicina_stavke),RIGHT(1),TIP('Ne moze biti vise od 99')
                       ENTRY(@n-14.2),AT(70,76,60,10),USE(STA:Iznos_stavke),DECIMAL(12)
                       BUTTON('Izracunaj'),AT(192,72,42),USE(?BUTTON1)
                       BUTTON('OK'),AT(8,106,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Prekini'),AT(54,106,40,12),USE(?Cancel)
                       STRING(@S40),AT(102,108),USE(ActionMessage)
                       PROMPT('Redni broj stavke:'),AT(5,9),USE(?STA:Redni_broj_stavk:Prompt)
                       PROMPT('Kolicina stavke:'),AT(5,50),USE(?STA:Kolicina_stavke:Prompt)
                       PROMPT('Iznos stavke:'),AT(5,76),USE(?STA:Iznos_stavke:Prompt)
                       PROMPT('Sifra artikla:'),AT(5,30),USE(?STA:Sifra_artikla:Prompt)
                       STRING(@s49),AT(144,30),USE(ART:Naziv_artikla)
                       STRING(@n-14.2),AT(70,90),USE(STA:Iznos_stavke,,?STA:Iznos_stavke:2)
                       BUTTON('USD'),AT(144,52,42,14),USE(?BUTTON1:2)
                       BUTTON('EUR'),AT(192,52,42,14),USE(?BUTTON1:3)
                       BUTTON('YEN'),AT(144,72,42,14),USE(?BUTTON1:4)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pogledaj zapis'
  OF InsertRecord
    ActionMessage = 'Zapis ce biti dodan'
  OF ChangeRecord
    ActionMessage = 'Zapis ce biti promijenjen'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeStavke')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?STA:Redni_broj_stavke
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(STA:Record,History::STA:Record)
  SELF.AddHistoryField(?STA:Redni_broj_stavke,1)
  SELF.AddHistoryField(?STA:Sifra_artikla,5)
  SELF.AddHistoryField(?STA:Kolicina_stavke,2)
  SELF.AddHistoryField(?STA:Iznos_stavke,3)
  SELF.AddHistoryField(?STA:Iznos_stavke:2,3)
  SELF.AddUpdateFile(Access:STAVKA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:STAVKA.Open                                       ! File STAVKA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:STAVKA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeStavke',FormWindow)              ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:STAVKA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeStavke',FormWindow)           ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  ART:Sifra_artikla = STA:Sifra_artikla                    ! Assign linking field value
  Access:ARTIKL.Fetch(ART:PK_Artikl)
  ART:Sifra_artikla = STA:Sifra_artikla                    ! Assign linking field value
  Access:ARTIKL.Fetch(ART:PK_Artikl)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PregledArtikala
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?BUTTON1
      STA:Iznos_stavke=STA:Kolicina_stavke*STA:Iznos_stavke
    OF ?BUTTON1:2
      STA:Iznos_stavke=STA:Iznos_stavke*6.25
    OF ?BUTTON1:3
      STA:Iznos_stavke=STA:Iznos_stavke*7.55
    OF ?BUTTON1:4
      STA:Iznos_stavke=STA:Iznos_stavke*0.06
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?STA:Sifra_artikla
      ART:Sifra_artikla = STA:Sifra_artikla
      IF Access:ARTIKL.TryFetch(ART:PK_Artikl)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          STA:Sifra_artikla = ART:Sifra_artikla
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
IspisMjesta PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(MJESTO)
                       PROJECT(MJE:Naziv_mjesta)
                       PROJECT(MJE:Postanski_broj)
                     END
ReportPageNumber     LONG,AUTO
ProgressWindow       WINDOW('Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2000,6500,7000),PRE(RPT),PAPER(PAPER:LETTER),FONT('Arial',10,,FONT:regular, |
  CHARSET:ANSI),THOUS
                       HEADER,AT(1000,1000,6500,1000),USE(?Header)
                         IMAGE('Backup\CroatiaBooksTextwh.jpg'),AT(135,31,1021,896),USE(?IMAGE1)
                         STRING('POPIS MJESTA'),AT(3812,115,,396),USE(?STRING1),FONT('Gill Sans Ultra Bold',20)
                         STRING('Vrijeme izvjesca:'),AT(1333,729),USE(?ReportTimePrompt),TRN
                         STRING('<<-- Time Stamp -->'),AT(2458,729),USE(?ReportTimeStamp),TRN
                         STRING('Datum izvjesca:'),AT(1333,469),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(2458,469),USE(?ReportDateStamp),TRN
                       END
Detail                 DETAIL,AT(0,0,6500,458),USE(?Detail)
                         STRING(@s5),AT(1510,135),USE(MJE:Postanski_broj)
                         STRING(@s49),AT(2083,135),USE(MJE:Naziv_mjesta)
                       END
                       FOOTER,AT(1000,9000,6500,427),USE(?Footer)
                         STRING(@N3),AT(5937,156),USE(ReportPageNumber)
                         STRING('Stranica:'),AT(5323,156),USE(?STRING2)
                       END
                       FORM,AT(1000,1000,6500,9000),USE(?Form)
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
  GlobalErrors.SetProcedureName('IspisMjesta')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:MJESTO.SetOpenRelated()
  Relate:MJESTO.Open                                       ! File MJESTO used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisMjesta',ProgressWindow)               ! Restore window settings from non-volatile store
  ThisReport.Init(Process:View, Relate:MJESTO, ?Progress:PctText, Progress:Thermometer)
  ThisReport.AddSortOrder()
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:MJESTO.SetQuickScan(1,Propagate:OneMany)
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
    Relate:MJESTO.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisMjesta',ProgressWindow)            ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OpenReport PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.OpenReport()
  IF ReturnValue = Level:Benign
    Report$?ReportPageNumber{PROP:PageNo} = True
  END
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportTimeStamp{PROP:Text} = FORMAT(CLOCK(),@T7)
  END
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportDateStamp{PROP:Text} = FORMAT(TODAY(),@D17)
  END
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:Detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
IspisNarudzbi PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(NARUDZBENICA)
                       PROJECT(NAR:Broj_narudzbe)
                       PROJECT(NAR:Datum)
                       PROJECT(NAR:Napomena)
                       PROJECT(NAR:PDV)
                       PROJECT(NAR:Sveukupno)
                       PROJECT(NAR:Sifra_izdavacke_kuce)
                       PROJECT(NAR:Sifra_nacina_dostave)
                       PROJECT(NAR:OIB_narucitelja)
                       JOIN(IZD:PK_IzdavackaKuca,NAR:Sifra_izdavacke_kuce)
                         PROJECT(IZD:Adresa_izdavacke_kuce)
                         PROJECT(IZD:Email_izdavacke_kuce)
                         PROJECT(IZD:IBAN_izdavacke_kuce)
                         PROJECT(IZD:Naziv_izdavacke_kuce)
                         PROJECT(IZD:Postanski_broj)
                         PROJECT(IZD:Telefon_izdavacke_kuce)
                         JOIN(MJE:PK_Mjesto,IZD:Postanski_broj)
                           PROJECT(MJE:Naziv_mjesta)
                         END
                       END
                       JOIN(NAC:PK_NacinDostave,NAR:Sifra_nacina_dostave)
                         PROJECT(NAC:Naziv_nacina_dostave)
                       END
                       JOIN(NARU:PK_Narucitelj,NAR:OIB_narucitelja)
                         PROJECT(NARU:Adresa_narucitelja)
                         PROJECT(NARU:Ime_i_prezime_narucitelja)
                         PROJECT(NARU:Postanski_broj)
                         PROJECT(NARU:Telefon_narucitelja)
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
Stanka                 BREAK(NARU:OIB_narucitelja),USE(?BREAK1)
StankaNar                BREAK(NAR:Broj_narudzbe),USE(?BREAK2)
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
Detail                     DETAIL,AT(0,0,6250,333),USE(?Detail)
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
  GlobalErrors.SetProcedureName('IspisNarudzbi')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:NARUDZBENICA.SetOpenRelated()
  Relate:NARUDZBENICA.Open                                 ! File NARUDZBENICA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisNarudzbi',ProgressWindow)             ! Restore window settings from non-volatile store
  ThisReport.Init(Process:View, Relate:NARUDZBENICA, ?Progress:PctText, Progress:Thermometer)
  ThisReport.AddSortOrder()
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:NARUDZBENICA.SetQuickScan(1,Propagate:OneMany)
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
    Relate:NARUDZBENICA.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisNarudzbi',ProgressWindow)          ! Save window data to non-volatile store
  END
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

