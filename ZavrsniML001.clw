

   MEMBER('ZavrsniML.clw')                                 ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABEIP.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('ZAVRSNIML001.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('ZAVRSNIML002.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('ZAVRSNIML003.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Frame
!!! Glavni izbornik
!!! </summary>
Main PROCEDURE 

SplashProcedureThread LONG
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('Croatia Books'),AT(,,387,243),FONT('Cooper Black',9,,,CHARSET:DEFAULT),RESIZE, |
  MAX,STATUS(-1,80,120,45),SYSTEM,WALLPAPER('Backup\CroatiaBooksbg.jpg'),IMM
                       MENUBAR,USE(?MENUBAR1)
                         MENU('&Datoteka'),USE(?FileMenu),COLOR(00E1FFFFh,COLOR:White,008B8B00h),ICON(ICON:Pick)
                           ITEM('P&ostavke ispisa'),USE(?PrintSetup),MSG('Setup Printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('Z&atvori'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('P&regled'),USE(?MENU3),COLOR(00E1FFFFh,,008B8B00h),ICON(ICON:Zoom)
                           ITEM('Narudzbenica'),USE(?PregledNarudzbenica)
                           ITEM('Narucitelja'),USE(?PregledNarucitelja)
                           ITEM('Izdavackih kuca'),USE(?PregledIK)
                           ITEM('Artikala'),USE(?PregledArtikala)
                           ITEM('Nacina dostave'),USE(?PregledND)
                           ITEM('Jedinica mjere'),USE(?PregledJM)
                           ITEM('Mjesta'),USE(?PregledMjesta)
                         END
                         MENU('&Izvjesca'),USE(?MENU4),COLOR(00E1FFFFh,COLOR:WINDOW,008B8B00h),ICON(ICON:Print)
                           ITEM('Mjesta'),USE(?IspisMjesta)
                           ITEM('Narudzbi'),USE(?IspisNarudzbi)
                         END
                         MENU('&Uredi'),USE(?EditMenu),COLOR(00E1FFFFh,COLOR:WINDOW,008B8B00h),ICON(ICON:Copy)
                           ITEM('Izrezi'),USE(?Cut),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                           ITEM('Kopiraj'),USE(?Copy),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                           ITEM('Zalijepi'),USE(?Paste),MSG('Paste contents of Windows Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Prozor'),USE(?MENU1),COLOR(00E1FFFFh,,008B8B00h),ICON(ICON:Clarion),MSG('Create and' & |
  ' Arrange windows'),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow)
                           ITEM('&Poslozi ikone'),USE(?Arrange),MSG('Align all window icons'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Pomoc'),USE(?MENU2),COLOR(00E1FFFFh,,008B8B00h),ICON(ICON:Help),MSG('Windows Help')
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'),STD(STD:HelpOnHelp)
                         END
                       END
                       TOOLBAR,AT(0,14,387,30),USE(?TOOLBAR1)
                         BUTTON('Narudzbenice'),AT(40,6,56,16),USE(?GumbNarudzbenica)
                         BUTTON('Narucitelji'),AT(99,6,56,16),USE(?BUTTON1:2)
                         BUTTON('Artikli'),AT(158,6,56,16),USE(?GumbArtikli)
                         BUTTON('Mjesta'),AT(217,6,56,16),USE(?GumbMjesta)
                         BUTTON('Izdavacke kuce'),AT(276,6,56,16),USE(?GumbMjesta:2)
                       END
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Menu::MENUBAR1 ROUTINE                                     ! Code for menu items on ?MENUBAR1
Menu::FileMenu ROUTINE                                     ! Code for menu items on ?FileMenu
Menu::MENU3 ROUTINE                                        ! Code for menu items on ?MENU3
  CASE ACCEPTED()
  OF ?PregledNarudzbenica
    START(PregledNarudzbenica, 25000)
  OF ?PregledNarucitelja
    START(PregledNarucitelja, 25000)
  OF ?PregledIK
    START(PregledIK, 25000)
  OF ?PregledArtikala
    START(PregledArtikala, 25000)
  OF ?PregledND
    START(PregledND, 25000)
  OF ?PregledJM
    START(PregledJM, 25000)
  OF ?PregledMjesta
    START(PregledMjesta, 25000)
  END
Menu::MENU4 ROUTINE                                        ! Code for menu items on ?MENU4
  CASE ACCEPTED()
  OF ?IspisMjesta
    START(IspisMjesta, 25000)
  OF ?IspisNarudzbi
    START(IspisNarudzbi, 25000)
  END
Menu::EditMenu ROUTINE                                     ! Code for menu items on ?EditMenu
Menu::MENU1 ROUTINE                                        ! Code for menu items on ?MENU1
Menu::MENU2 ROUTINE                                        ! Code for menu items on ?MENU2

ThisWindow.Ask PROCEDURE

  CODE
  IF NOT INRANGE(AppFrame{PROP:Timer},1,100)
    AppFrame{PROP:Timer} = 100
  END
    AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D6)
    AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T4)
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('Main',AppFrame)                            ! Restore window settings from non-volatile store
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('Main',AppFrame)                         ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
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
    ELSE
      DO Menu::MENUBAR1                                    ! Process menu items on ?MENUBAR1 menu
      DO Menu::FileMenu                                    ! Process menu items on ?FileMenu menu
      DO Menu::MENU3                                       ! Process menu items on ?MENU3 menu
      DO Menu::MENU4                                       ! Process menu items on ?MENU4 menu
      DO Menu::EditMenu                                    ! Process menu items on ?EditMenu menu
      DO Menu::MENU1                                       ! Process menu items on ?MENU1 menu
      DO Menu::MENU2                                       ! Process menu items on ?MENU2 menu
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?GumbNarudzbenica
      START(PregledNarudzbenica, 25000)
    OF ?BUTTON1:2
      START(PregledNarucitelja, 25000)
    OF ?GumbArtikli
      START(PregledArtikala, 25000)
    OF ?GumbMjesta
      START(PregledMjesta, 25000)
    OF ?GumbMjesta:2
      START(AzuriranjeIK, 25000)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      SplashProcedureThread = START(SkocniProzor)          ! Run the splash window procedure
    OF EVENT:Timer
      AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D6)
      AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T4)
    ELSE
      IF SplashProcedureThread
        IF EVENT() = Event:Accepted
          POST(Event:CloseWindow,,SplashProcedureThread)   ! Close the splash window
          SplashPRocedureThread = 0
        END
     END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SkocniProzor PROCEDURE 

window               WINDOW,AT(,,204,112),FONT('Microsoft Sans Serif',8,,FONT:regular),NOFRAME,CENTER,GRAY,MDI
                       STRING('Dobrodosli!'),AT(8,22,106,19),USE(?String2:2),FONT('Bauhaus 93',20),CENTER
                       IMAGE('Backup\418433409250099201.gif'),AT(48,78,22,20),USE(?IMAGE2)
                       IMAGE('Backup\CroatiaBooksText.png'),AT(107,13,83,85),USE(?IMAGE2:2)
                       IMAGE('Backup\6137_croatiaparrot.gif'),AT(19,44,22,20),USE(?IMAGE2:3)
                       IMAGE('Backup\4985_thumbs_up.gif'),AT(48,44,22,20),USE(?IMAGE2:4)
                       IMAGE('Backup\7773_tunez_rainbow.gif'),AT(81,44,22,20),USE(?IMAGE2:5)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

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
  GlobalErrors.SetProcedureName('SkocniProzor')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?String2:2
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('SkocniProzor',window)                      ! Restore window settings from non-volatile store
  TARGET{Prop:Timer} = 500                                 ! Close window on timer event, so configure timer
  TARGET{Prop:Alrt,255} = MouseLeft                        ! Alert mouse clicks that will close window
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('SkocniProzor',window)                   ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)                            ! Splash window will close on mouse click
      END
    OF EVENT:LoseFocus
        POST(Event:CloseWindow)                            ! Splash window will close when focus is lost
    OF Event:Timer
      POST(Event:CloseWindow)                              ! Splash window will close on event timer
    OF Event:AlertKey
      CASE KEYCODE()                                       ! Splash window will close on mouse click
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledArtikala PROCEDURE 

BRW1::View:Browse    VIEW(ARTIKL)
                       PROJECT(ART:Sifra_artikla)
                       PROJECT(ART:Naziv_artikla)
                       PROJECT(ART:Sifra_jedinice_mjere)
                       JOIN(JED:PK_JedinicaMjere,ART:Sifra_jedinice_mjere)
                         PROJECT(JED:Naziv_jedinice_mjere)
                         PROJECT(JED:Sifra_jedinice_mjere)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
ART:Sifra_artikla      LIKE(ART:Sifra_artikla)        !List box control field - type derived from field
ART:Naziv_artikla      LIKE(ART:Naziv_artikla)        !List box control field - type derived from field
JED:Naziv_jedinice_mjere LIKE(JED:Naziv_jedinice_mjere) !List box control field - type derived from field
JED:Sifra_jedinice_mjere LIKE(JED:Sifra_jedinice_mjere) !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Pregled artikala'),AT(0,0,323,148),GRAY,MDI,SYSTEM
                       LIST,AT(9,20,306,100),USE(?List),HVSCROLL,FORMAT('49C(2)|M~Sifra artikla~C(1)@n-7@173C(' & |
  '2)|M~Naziv artikla~C(0)@s49@76C(2)|M~Naziv jedinice mjere~C(0)@s19@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(9,126,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(54,126,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(99,126,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(149,126,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(204,126,40,12),USE(?Close)
                       SHEET,AT(8,6,307,115),USE(?SHEET1)
                         TAB('Po sifri'),USE(?TAB1)
                         END
                         TAB('Po imenu'),USE(?TAB2)
                         END
                       END
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?SHEET1)=2

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
  GlobalErrors.SetProcedureName('PregledArtikala')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:ARTIKL.SetOpenRelated()
  Relate:ARTIKL.Open                                       ! File ARTIKL used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:ARTIKL,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,ART:SK_ArtiklNaziv)                   ! Add the sort order for ART:SK_ArtiklNaziv for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,ART:Naziv_artikla,1,BRW1)      ! Initialize the browse locator using  using key: ART:SK_ArtiklNaziv , ART:Naziv_artikla
  BRW1.AddSortOrder(,ART:PK_Artikl)                        ! Add the sort order for ART:PK_Artikl for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,ART:Sifra_artikla,1,BRW1)      ! Initialize the browse locator using  using key: ART:PK_Artikl , ART:Sifra_artikla
  BRW1.AddField(ART:Sifra_artikla,BRW1.Q.ART:Sifra_artikla) ! Field ART:Sifra_artikla is a hot field or requires assignment from browse
  BRW1.AddField(ART:Naziv_artikla,BRW1.Q.ART:Naziv_artikla) ! Field ART:Naziv_artikla is a hot field or requires assignment from browse
  BRW1.AddField(JED:Naziv_jedinice_mjere,BRW1.Q.JED:Naziv_jedinice_mjere) ! Field JED:Naziv_jedinice_mjere is a hot field or requires assignment from browse
  BRW1.AddField(JED:Sifra_jedinice_mjere,BRW1.Q.JED:Sifra_jedinice_mjere) ! Field JED:Sifra_jedinice_mjere is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledArtikala',BrowseWindow)             ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeArtikala
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledArtikala',BrowseWindow)          ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeArtikala
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?SHEET1)=2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledMjesta PROCEDURE 

BRW1::View:Browse    VIEW(MJESTO)
                       PROJECT(MJE:Postanski_broj)
                       PROJECT(MJE:Naziv_mjesta)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
MJE:Postanski_broj     LIKE(MJE:Postanski_broj)       !List box control field - type derived from field
MJE:Naziv_mjesta       LIKE(MJE:Naziv_mjesta)         !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis mjesta'),AT(0,0,247,140),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('61C(2)|M~Postanski broj~C(0)@s5@196C(2' & |
  ')|M~Naziv mjesta~C(0)@s49@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::EIPManager     BrowseEIPManager                      ! Browse EIP Manager for Browse using ?List
EditInPlace::MJE:Postanski_broj EditEntryClass             ! Edit-in-place class for field MJE:Postanski_broj
EditInPlace::MJE:Naziv_mjesta EditEntryClass               ! Edit-in-place class for field MJE:Naziv_mjesta

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
  GlobalErrors.SetProcedureName('PregledMjesta')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:MJESTO.SetOpenRelated()
  Relate:MJESTO.Open                                       ! File MJESTO used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:MJESTO,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,MJE:PK_Mjesto)                        ! Add the sort order for MJE:PK_Mjesto for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,MJE:Postanski_broj,1,BRW1)     ! Initialize the browse locator using  using key: MJE:PK_Mjesto , MJE:Postanski_broj
  BRW1.AddField(MJE:Postanski_broj,BRW1.Q.MJE:Postanski_broj) ! Field MJE:Postanski_broj is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Naziv_mjesta,BRW1.Q.MJE:Naziv_mjesta)  ! Field MJE:Naziv_mjesta is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledMjesta',BrowseWindow)               ! Restore window settings from non-volatile store
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledMjesta',BrowseWindow)            ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeMjesta
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  SELF.EIP &= BRW1::EIPManager                             ! Set the EIP manager
  SELF.AddEditControl(EditInPlace::MJE:Postanski_broj,1)
  SELF.AddEditControl(EditInPlace::MJE:Naziv_mjesta,2)
  SELF.DeleteAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledNarudzbenica PROCEDURE 

BRW1::View:Browse    VIEW(NARUDZBENICA)
                       PROJECT(NAR:Broj_narudzbe)
                       PROJECT(NAR:Datum)
                       PROJECT(NAR:PDV)
                       PROJECT(NAR:Sveukupno)
                       PROJECT(NAR:Napomena)
                       PROJECT(NAR:Sifra_izdavacke_kuce)
                       PROJECT(NAR:Sifra_nacina_dostave)
                       PROJECT(NAR:OIB_narucitelja)
                       JOIN(IZD:PK_IzdavackaKuca,NAR:Sifra_izdavacke_kuce)
                         PROJECT(IZD:Naziv_izdavacke_kuce)
                         PROJECT(IZD:Sifra_izdavacke_kuce)
                       END
                       JOIN(NAC:PK_NacinDostave,NAR:Sifra_nacina_dostave)
                         PROJECT(NAC:Naziv_nacina_dostave)
                         PROJECT(NAC:Sifra_nacina_dostave)
                       END
                       JOIN(NARU:PK_Narucitelj,NAR:OIB_narucitelja)
                         PROJECT(NARU:Ime_i_prezime_narucitelja)
                         PROJECT(NARU:OIB_narucitelja)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
NAR:Broj_narudzbe      LIKE(NAR:Broj_narudzbe)        !List box control field - type derived from field
NAR:Datum              LIKE(NAR:Datum)                !List box control field - type derived from field
NAR:PDV                LIKE(NAR:PDV)                  !List box control field - type derived from field
NAR:Sveukupno          LIKE(NAR:Sveukupno)            !List box control field - type derived from field
NARU:Ime_i_prezime_narucitelja LIKE(NARU:Ime_i_prezime_narucitelja) !List box control field - type derived from field
NAC:Naziv_nacina_dostave LIKE(NAC:Naziv_nacina_dostave) !List box control field - type derived from field
IZD:Naziv_izdavacke_kuce LIKE(IZD:Naziv_izdavacke_kuce) !List box control field - type derived from field
NAR:Napomena           LIKE(NAR:Napomena)             !List box control field - type derived from field
IZD:Sifra_izdavacke_kuce LIKE(IZD:Sifra_izdavacke_kuce) !Related join file key field - type derived from field
NAC:Sifra_nacina_dostave LIKE(NAC:Sifra_nacina_dostave) !Related join file key field - type derived from field
NARU:OIB_narucitelja   LIKE(NARU:OIB_narucitelja)     !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Pregled narudzbenica'),AT(0,0,509,180),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,502,102),USE(?List),HVSCROLL,FORMAT('47C(2)|M~Broj narudzbe~C(0)@P#{9}P@43C' & |
  '|M~Datum~@d17@49C|M~PDV~@n-14.2@51C|M~Sveukupno~@n-14.2@88C(2)|M~Ime i prezime naruc' & |
  'itelja~C(0)@s49@51C(2)|M~Nacin dostave~C(0)@s49@55C(2)|M~Izdavacka kuca~C(0)@s49@101' & |
  '6L(2)|M~Napomena~C(0)@s254@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                       BUTTON('Ispisi specificnu tablicu'),AT(378,110,124,62),USE(?Print),FONT(,13,COLOR:INACTIVEBORDER, |
  FONT:bold+FONT:underline),COLOR(COLOR:Black),ICON('Backup\pti.gif'),CURSOR(CURSOR:Zoom)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PregledNarudzbenica')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:NARUDZBENICA.SetOpenRelated()
  Relate:NARUDZBENICA.Open                                 ! File NARUDZBENICA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:NARUDZBENICA,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,NAR:PK_Narudzbenica)                  ! Add the sort order for NAR:PK_Narudzbenica for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,NAR:Broj_narudzbe,1,BRW1)      ! Initialize the browse locator using  using key: NAR:PK_Narudzbenica , NAR:Broj_narudzbe
  BRW1.AddField(NAR:Broj_narudzbe,BRW1.Q.NAR:Broj_narudzbe) ! Field NAR:Broj_narudzbe is a hot field or requires assignment from browse
  BRW1.AddField(NAR:Datum,BRW1.Q.NAR:Datum)                ! Field NAR:Datum is a hot field or requires assignment from browse
  BRW1.AddField(NAR:PDV,BRW1.Q.NAR:PDV)                    ! Field NAR:PDV is a hot field or requires assignment from browse
  BRW1.AddField(NAR:Sveukupno,BRW1.Q.NAR:Sveukupno)        ! Field NAR:Sveukupno is a hot field or requires assignment from browse
  BRW1.AddField(NARU:Ime_i_prezime_narucitelja,BRW1.Q.NARU:Ime_i_prezime_narucitelja) ! Field NARU:Ime_i_prezime_narucitelja is a hot field or requires assignment from browse
  BRW1.AddField(NAC:Naziv_nacina_dostave,BRW1.Q.NAC:Naziv_nacina_dostave) ! Field NAC:Naziv_nacina_dostave is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Naziv_izdavacke_kuce,BRW1.Q.IZD:Naziv_izdavacke_kuce) ! Field IZD:Naziv_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(NAR:Napomena,BRW1.Q.NAR:Napomena)          ! Field NAR:Napomena is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Sifra_izdavacke_kuce,BRW1.Q.IZD:Sifra_izdavacke_kuce) ! Field IZD:Sifra_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(NAC:Sifra_nacina_dostave,BRW1.Q.NAC:Sifra_nacina_dostave) ! Field NAC:Sifra_nacina_dostave is a hot field or requires assignment from browse
  BRW1.AddField(NARU:OIB_narucitelja,BRW1.Q.NARU:OIB_narucitelja) ! Field NARU:OIB_narucitelja is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledNarudzbenica',BrowseWindow)         ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeNarudzbenice
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.PrintProcedure = 2
  BRW1.PrintControl = ?Print
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
    INIMgr.Update('PregledNarudzbenica',BrowseWindow)      ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
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
      AzuriranjeNarudzbenice
      IspisOdredeneNarudzbe
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledNarucitelja PROCEDURE 

BRW1::View:Browse    VIEW(NARUCITELJ)
                       PROJECT(NARU:OIB_narucitelja)
                       PROJECT(NARU:Ime_i_prezime_narucitelja)
                       PROJECT(NARU:Adresa_narucitelja)
                       PROJECT(NARU:Telefon_narucitelja)
                       PROJECT(NARU:Postanski_broj)
                       JOIN(MJE:PK_Mjesto,NARU:Postanski_broj)
                         PROJECT(MJE:Naziv_mjesta)
                         PROJECT(MJE:Postanski_broj)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
NARU:OIB_narucitelja   LIKE(NARU:OIB_narucitelja)     !List box control field - type derived from field
NARU:Ime_i_prezime_narucitelja LIKE(NARU:Ime_i_prezime_narucitelja) !List box control field - type derived from field
NARU:Adresa_narucitelja LIKE(NARU:Adresa_narucitelja) !List box control field - type derived from field
NARU:Telefon_narucitelja LIKE(NARU:Telefon_narucitelja) !List box control field - type derived from field
MJE:Naziv_mjesta       LIKE(MJE:Naziv_mjesta)         !List box control field - type derived from field
MJE:Postanski_broj     LIKE(MJE:Postanski_broj)       !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis narucitelja'),AT(0,0,494,140),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,469,100),USE(?List),HVSCROLL,FORMAT('59C(2)|M~OIB narucitelja~C(0)@s11@102C' & |
  '(2)|M~Ime i prezime narucitelja~C(0)@s49@120C(2)|M~Adresa narucitelja~C(0)@s49@85C(2' & |
  ')|M~Telefon narucitelja~C(0)@P###/###-###P@196C(2)|M~Naziv mjesta~C(0)@s49@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PregledNarucitelja')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:NARUCITELJ.SetOpenRelated()
  Relate:NARUCITELJ.Open                                   ! File NARUCITELJ used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:NARUCITELJ,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,NARU:PK_Narucitelj)                   ! Add the sort order for NARU:PK_Narucitelj for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,NARU:OIB_narucitelja,1,BRW1)   ! Initialize the browse locator using  using key: NARU:PK_Narucitelj , NARU:OIB_narucitelja
  BRW1.AddField(NARU:OIB_narucitelja,BRW1.Q.NARU:OIB_narucitelja) ! Field NARU:OIB_narucitelja is a hot field or requires assignment from browse
  BRW1.AddField(NARU:Ime_i_prezime_narucitelja,BRW1.Q.NARU:Ime_i_prezime_narucitelja) ! Field NARU:Ime_i_prezime_narucitelja is a hot field or requires assignment from browse
  BRW1.AddField(NARU:Adresa_narucitelja,BRW1.Q.NARU:Adresa_narucitelja) ! Field NARU:Adresa_narucitelja is a hot field or requires assignment from browse
  BRW1.AddField(NARU:Telefon_narucitelja,BRW1.Q.NARU:Telefon_narucitelja) ! Field NARU:Telefon_narucitelja is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Naziv_mjesta,BRW1.Q.MJE:Naziv_mjesta)  ! Field MJE:Naziv_mjesta is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Postanski_broj,BRW1.Q.MJE:Postanski_broj) ! Field MJE:Postanski_broj is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledNarucitelja',BrowseWindow)          ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeNarucitelja
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledNarucitelja',BrowseWindow)       ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeNarucitelja
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledIK PROCEDURE 

BRW1::View:Browse    VIEW(IZDAVACKA_KUCA)
                       PROJECT(IZD:Sifra_izdavacke_kuce)
                       PROJECT(IZD:Naziv_izdavacke_kuce)
                       PROJECT(IZD:Adresa_izdavacke_kuce)
                       PROJECT(IZD:Email_izdavacke_kuce)
                       PROJECT(IZD:Telefon_izdavacke_kuce)
                       PROJECT(IZD:IBAN_izdavacke_kuce)
                       PROJECT(IZD:Postanski_broj)
                       JOIN(MJE:PK_Mjesto,IZD:Postanski_broj)
                         PROJECT(MJE:Naziv_mjesta)
                         PROJECT(MJE:Postanski_broj)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
IZD:Sifra_izdavacke_kuce LIKE(IZD:Sifra_izdavacke_kuce) !List box control field - type derived from field
IZD:Naziv_izdavacke_kuce LIKE(IZD:Naziv_izdavacke_kuce) !List box control field - type derived from field
IZD:Adresa_izdavacke_kuce LIKE(IZD:Adresa_izdavacke_kuce) !List box control field - type derived from field
IZD:Email_izdavacke_kuce LIKE(IZD:Email_izdavacke_kuce) !List box control field - type derived from field
IZD:Telefon_izdavacke_kuce LIKE(IZD:Telefon_izdavacke_kuce) !List box control field - type derived from field
IZD:IBAN_izdavacke_kuce LIKE(IZD:IBAN_izdavacke_kuce) !List box control field - type derived from field
MJE:Naziv_mjesta       LIKE(MJE:Naziv_mjesta)         !List box control field - type derived from field
MJE:Postanski_broj     LIKE(MJE:Postanski_broj)       !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis izdavackih kuca'),AT(0,0,560,140),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,548,100),USE(?List),HVSCROLL,FORMAT('65C(2)|M~Sifra izdavacke kuce~C(0)@P##' & |
  '###P@74C(2)|M~Naziv izdavacke kuce~C(0)@s49@85C(2)|M~Adresa izdavacke kuce~C(0)@s49@' & |
  '71C(2)|M~Email izdavacke kuce~C(0)@s49@76C(2)|M~Telefon izdavacke kuce~C(0)@P###/###' & |
  '-###P@86C(2)|M~IBAN izdavacke kuce~C(0)@s21@196C(2)|M~Naziv mjesta~C(0)@s49@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PregledIK')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:IZDAVACKA_KUCA.SetOpenRelated()
  Relate:IZDAVACKA_KUCA.Open                               ! File IZDAVACKA_KUCA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:IZDAVACKA_KUCA,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,IZD:PK_IzdavackaKuca)                 ! Add the sort order for IZD:PK_IzdavackaKuca for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,IZD:Sifra_izdavacke_kuce,1,BRW1) ! Initialize the browse locator using  using key: IZD:PK_IzdavackaKuca , IZD:Sifra_izdavacke_kuce
  BRW1.AddField(IZD:Sifra_izdavacke_kuce,BRW1.Q.IZD:Sifra_izdavacke_kuce) ! Field IZD:Sifra_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Naziv_izdavacke_kuce,BRW1.Q.IZD:Naziv_izdavacke_kuce) ! Field IZD:Naziv_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Adresa_izdavacke_kuce,BRW1.Q.IZD:Adresa_izdavacke_kuce) ! Field IZD:Adresa_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Email_izdavacke_kuce,BRW1.Q.IZD:Email_izdavacke_kuce) ! Field IZD:Email_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(IZD:Telefon_izdavacke_kuce,BRW1.Q.IZD:Telefon_izdavacke_kuce) ! Field IZD:Telefon_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(IZD:IBAN_izdavacke_kuce,BRW1.Q.IZD:IBAN_izdavacke_kuce) ! Field IZD:IBAN_izdavacke_kuce is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Naziv_mjesta,BRW1.Q.MJE:Naziv_mjesta)  ! Field MJE:Naziv_mjesta is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Postanski_broj,BRW1.Q.MJE:Postanski_broj) ! Field MJE:Postanski_broj is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledIK',BrowseWindow)                   ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeIK
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledIK',BrowseWindow)                ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeIK
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledND PROCEDURE 

BRW1::View:Browse    VIEW(NACIN_DOSTAVE)
                       PROJECT(NAC:Sifra_nacina_dostave)
                       PROJECT(NAC:Naziv_nacina_dostave)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
NAC:Sifra_nacina_dostave LIKE(NAC:Sifra_nacina_dostave) !List box control field - type derived from field
NAC:Naziv_nacina_dostave LIKE(NAC:Naziv_nacina_dostave) !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Pregled nacina dostave'),AT(0,0,247,140),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('76L(2)|M~Sifra nacina dostave~L(0)@P##' & |
  '###P@196L(2)|M~Naziv nacina dostave~L(0)@s49@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Unesi'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PregledND')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:NACIN_DOSTAVE.Open                                ! File NACIN_DOSTAVE used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:NACIN_DOSTAVE,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,NAC:PK_NacinDostave)                  ! Add the sort order for NAC:PK_NacinDostave for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,NAC:Sifra_nacina_dostave,1,BRW1) ! Initialize the browse locator using  using key: NAC:PK_NacinDostave , NAC:Sifra_nacina_dostave
  BRW1.AddField(NAC:Sifra_nacina_dostave,BRW1.Q.NAC:Sifra_nacina_dostave) ! Field NAC:Sifra_nacina_dostave is a hot field or requires assignment from browse
  BRW1.AddField(NAC:Naziv_nacina_dostave,BRW1.Q.NAC:Naziv_nacina_dostave) ! Field NAC:Naziv_nacina_dostave is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledND',BrowseWindow)                   ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeND
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledND',BrowseWindow)                ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeND
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PregledJM PROCEDURE 

BRW1::View:Browse    VIEW(JEDINICA_MJERE)
                       PROJECT(JED:Sifra_jedinice_mjere)
                       PROJECT(JED:Naziv_jedinice_mjere)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
JED:Sifra_jedinice_mjere LIKE(JED:Sifra_jedinice_mjere) !List box control field - type derived from field
JED:Naziv_jedinice_mjere LIKE(JED:Naziv_jedinice_mjere) !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Pregled jedinica mjere...'),AT(0,0,247,140),GRAY,MDI,SYSTEM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('68C(2)|M~Sifra jedinice mjere~C(1)@n-7' & |
  '@76C(2)|M~Naziv jedinice mjere~C(0)@s19@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Unesi<0DH,0AH>'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('&Promijeni'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('&Izbrisi'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('&Odaberi'),AT(145,110,40,12),USE(?Select)
                       BUTTON('Zatvori'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PregledJM')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:JEDINICA_MJERE.SetOpenRelated()
  Relate:JEDINICA_MJERE.Open                               ! File JEDINICA_MJERE used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:JEDINICA_MJERE,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,JED:PK_JedinicaMjere)                 ! Add the sort order for JED:PK_JedinicaMjere for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,JED:Sifra_jedinice_mjere,1,BRW1) ! Initialize the browse locator using  using key: JED:PK_JedinicaMjere , JED:Sifra_jedinice_mjere
  BRW1.AddField(JED:Sifra_jedinice_mjere,BRW1.Q.JED:Sifra_jedinice_mjere) ! Field JED:Sifra_jedinice_mjere is a hot field or requires assignment from browse
  BRW1.AddField(JED:Naziv_jedinice_mjere,BRW1.Q.JED:Naziv_jedinice_mjere) ! Field JED:Naziv_jedinice_mjere is a hot field or requires assignment from browse
  INIMgr.Fetch('PregledJM',BrowseWindow)                   ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeJM
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PregledJM',BrowseWindow)                ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeJM
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

