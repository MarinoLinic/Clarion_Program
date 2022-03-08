   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('ZAVRSNIML_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('ZAVRSNIML001.CLW')
Main                   PROCEDURE   !Glavni izbornik
     END
   END

SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
NARUDZBENICA         FILE,DRIVER('TOPSPEED'),PRE(NAR),CREATE,BINDABLE,THREAD !                     
PK_Narudzbenica          KEY(NAR:Broj_narudzbe),NOCASE,PRIMARY !                     
VK_NarudzbenicaOIB       KEY(NAR:OIB_narucitelja),DUP,NOCASE !                     
VK_NarudzbenicaIK        KEY(NAR:Sifra_izdavacke_kuce),DUP,NOCASE !                     
VK_NarudzbenicaND        KEY(NAR:Sifra_nacina_dostave),DUP,NOCASE !                     
Record                   RECORD,PRE()
Broj_narudzbe               STRING(20)                     !                     
Napomena                    CSTRING(255)                   !                     
Datum                       DATE                           !                     
PDV                         DECIMAL(10,2)                  !                     
Sveukupno                   DECIMAL(10,2)                  !                     
OIB_narucitelja             CSTRING(12)                    !                     
Sifra_izdavacke_kuce        STRING(20)                     !                     
Sifra_nacina_dostave        STRING(20)                     !                     
                         END
                     END                       

MJESTO               FILE,DRIVER('TOPSPEED'),PRE(MJE),CREATE,BINDABLE,THREAD !                     
PK_Mjesto                KEY(MJE:Postanski_broj),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
Postanski_broj              CSTRING(6)                     !                     
Naziv_mjesta                CSTRING(50)                    !                     
                         END
                     END                       

IZDAVACKA_KUCA       FILE,DRIVER('TOPSPEED'),PRE(IZD),CREATE,BINDABLE,THREAD !                     
PK_IzdavackaKuca         KEY(IZD:Sifra_izdavacke_kuce),NOCASE,PRIMARY !                     
VK_IzdavackaKuca         KEY(IZD:Postanski_broj),DUP,NOCASE !                     
Record                   RECORD,PRE()
Sifra_izdavacke_kuce        STRING(20)                     !                     
Naziv_izdavacke_kuce        CSTRING(50)                    !                     
Adresa_izdavacke_kuce       CSTRING(50)                    !                     
Email_izdavacke_kuce        CSTRING(50)                    !                     
Telefon_izdavacke_kuce      CSTRING(20)                    !                     
IBAN_izdavacke_kuce         CSTRING(22)                    !                     
Postanski_broj              CSTRING(6)                     !                     
                         END
                     END                       

NARUCITELJ           FILE,DRIVER('TOPSPEED'),PRE(NARU),CREATE,BINDABLE,THREAD !                     
PK_Narucitelj            KEY(NARU:OIB_narucitelja),NOCASE,PRIMARY !                     
VK_Narucitelj            KEY(NARU:Postanski_broj),DUP,NOCASE !                     
Record                   RECORD,PRE()
OIB_narucitelja             CSTRING(12)                    !                     
Ime_i_prezime_narucitelja   CSTRING(50)                    !                     
Adresa_narucitelja          CSTRING(50)                    !                     
Telefon_narucitelja         CSTRING(11)                    !                     
Postanski_broj              CSTRING(6)                     !                     
                         END
                     END                       

NACIN_DOSTAVE        FILE,DRIVER('TOPSPEED'),PRE(NAC),CREATE,BINDABLE,THREAD !                     
PK_NacinDostave          KEY(NAC:Sifra_nacina_dostave),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
Sifra_nacina_dostave        STRING(20)                     !                     
Naziv_nacina_dostave        CSTRING(50)                    !                     
                         END
                     END                       

JEDINICA_MJERE       FILE,DRIVER('TOPSPEED'),PRE(JED),CREATE,BINDABLE,THREAD !                     
PK_JedinicaMjere         KEY(JED:Sifra_jedinice_mjere),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
Sifra_jedinice_mjere        SHORT                          !                     
Naziv_jedinice_mjere        CSTRING(20)                    !                     
                         END
                     END                       

ARTIKL               FILE,DRIVER('TOPSPEED'),PRE(ART),CREATE,BINDABLE,THREAD !                     
PK_Artikl                KEY(ART:Sifra_artikla),NOCASE,PRIMARY !                     
VK_Artikl                KEY(ART:Sifra_jedinice_mjere),DUP,NOCASE,OPT !                     
SK_ArtiklNaziv           KEY(ART:Naziv_artikla),DUP,NOCASE !                     
Record                   RECORD,PRE()
Sifra_artikla               SHORT                          !                     
Naziv_artikla               CSTRING(50)                    !                     
Sifra_jedinice_mjere        SHORT                          !                     
                         END
                     END                       

STAVKA               FILE,DRIVER('TOPSPEED'),PRE(STA),CREATE,BINDABLE,THREAD !                     
PK_Stavka                KEY(STA:Broj_narudzbe,STA:Redni_broj_stavke),NOCASE,PRIMARY !                     
VK_Stavka                KEY(STA:Sifra_artikla),DUP,NOCASE !                     
Record                   RECORD,PRE()
Redni_broj_stavke           LONG                           !                     
Kolicina_stavke             SHORT                          !                     
Iznos_stavke                DECIMAL(10,2)                  !                     
Broj_narudzbe               STRING(20)                     !                     
Sifra_artikla               SHORT                          !                     
                         END
                     END                       

!endregion

Access:NARUDZBENICA  &FileManager,THREAD                   ! FileManager for NARUDZBENICA
Relate:NARUDZBENICA  &RelationManager,THREAD               ! RelationManager for NARUDZBENICA
Access:MJESTO        &FileManager,THREAD                   ! FileManager for MJESTO
Relate:MJESTO        &RelationManager,THREAD               ! RelationManager for MJESTO
Access:IZDAVACKA_KUCA &FileManager,THREAD                  ! FileManager for IZDAVACKA_KUCA
Relate:IZDAVACKA_KUCA &RelationManager,THREAD              ! RelationManager for IZDAVACKA_KUCA
Access:NARUCITELJ    &FileManager,THREAD                   ! FileManager for NARUCITELJ
Relate:NARUCITELJ    &RelationManager,THREAD               ! RelationManager for NARUCITELJ
Access:NACIN_DOSTAVE &FileManager,THREAD                   ! FileManager for NACIN_DOSTAVE
Relate:NACIN_DOSTAVE &RelationManager,THREAD               ! RelationManager for NACIN_DOSTAVE
Access:JEDINICA_MJERE &FileManager,THREAD                  ! FileManager for JEDINICA_MJERE
Relate:JEDINICA_MJERE &RelationManager,THREAD              ! RelationManager for JEDINICA_MJERE
Access:ARTIKL        &FileManager,THREAD                   ! FileManager for ARTIKL
Relate:ARTIKL        &RelationManager,THREAD               ! RelationManager for ARTIKL
Access:STAVKA        &FileManager,THREAD                   ! FileManager for STAVKA
Relate:STAVKA        &RelationManager,THREAD               ! RelationManager for STAVKA

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\ZavrsniML.INI', NVD_INI)                  ! Configure INIManager to use INI file
  DctInit
  Main
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

