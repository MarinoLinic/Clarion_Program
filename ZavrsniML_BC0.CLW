  MEMBER('ZavrsniML.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
ZavrsniML_BC0:DctInit    PROCEDURE()
ZavrsniML_BC0:DctKill    PROCEDURE()
ZavrsniML_BC0:FilesInit  PROCEDURE()
  END

Hide:Access:NARUDZBENICA CLASS(FileManager),TYPE           ! FileManager for NARUDZBENICA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:NARUDZBENICA CLASS(RelationManager),TYPE       ! RelationManager for NARUDZBENICA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:MJESTO   CLASS(FileManager),TYPE               ! FileManager for MJESTO
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:MJESTO   CLASS(RelationManager),TYPE           ! RelationManager for MJESTO
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:IZDAVACKA_KUCA CLASS(FileManager),TYPE         ! FileManager for IZDAVACKA_KUCA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:IZDAVACKA_KUCA CLASS(RelationManager),TYPE     ! RelationManager for IZDAVACKA_KUCA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:NARUCITELJ CLASS(FileManager),TYPE             ! FileManager for NARUCITELJ
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:NARUCITELJ CLASS(RelationManager),TYPE         ! RelationManager for NARUCITELJ
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:NACIN_DOSTAVE CLASS(FileManager),TYPE          ! FileManager for NACIN_DOSTAVE
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:NACIN_DOSTAVE CLASS(RelationManager),TYPE      ! RelationManager for NACIN_DOSTAVE
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:JEDINICA_MJERE CLASS(FileManager),TYPE         ! FileManager for JEDINICA_MJERE
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:JEDINICA_MJERE CLASS(RelationManager),TYPE     ! RelationManager for JEDINICA_MJERE
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:ARTIKL   CLASS(FileManager),TYPE               ! FileManager for ARTIKL
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:ARTIKL   CLASS(RelationManager),TYPE           ! RelationManager for ARTIKL
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:STAVKA   CLASS(FileManager),TYPE               ! FileManager for STAVKA
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:STAVKA   CLASS(RelationManager),TYPE           ! RelationManager for STAVKA
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:NARUDZBENICA &Hide:Access:NARUDZBENICA,AUTO,THREAD
_Hide:Relate:NARUDZBENICA &Hide:Relate:NARUDZBENICA,AUTO,THREAD
_Hide:Access:MJESTO  &Hide:Access:MJESTO,AUTO,THREAD
_Hide:Relate:MJESTO  &Hide:Relate:MJESTO,AUTO,THREAD
_Hide:Access:IZDAVACKA_KUCA &Hide:Access:IZDAVACKA_KUCA,AUTO,THREAD
_Hide:Relate:IZDAVACKA_KUCA &Hide:Relate:IZDAVACKA_KUCA,AUTO,THREAD
_Hide:Access:NARUCITELJ &Hide:Access:NARUCITELJ,AUTO,THREAD
_Hide:Relate:NARUCITELJ &Hide:Relate:NARUCITELJ,AUTO,THREAD
_Hide:Access:NACIN_DOSTAVE &Hide:Access:NACIN_DOSTAVE,AUTO,THREAD
_Hide:Relate:NACIN_DOSTAVE &Hide:Relate:NACIN_DOSTAVE,AUTO,THREAD
_Hide:Access:JEDINICA_MJERE &Hide:Access:JEDINICA_MJERE,AUTO,THREAD
_Hide:Relate:JEDINICA_MJERE &Hide:Relate:JEDINICA_MJERE,AUTO,THREAD
_Hide:Access:ARTIKL  &Hide:Access:ARTIKL,AUTO,THREAD
_Hide:Relate:ARTIKL  &Hide:Relate:ARTIKL,AUTO,THREAD
_Hide:Access:STAVKA  &Hide:Access:STAVKA,AUTO,THREAD
_Hide:Relate:STAVKA  &Hide:Relate:STAVKA,AUTO,THREAD


ZavrsniML_BC0:DctInit PROCEDURE()
  CODE
  _Hide:Access:NARUDZBENICA &= NEW(Hide:Access:NARUDZBENICA)
  _Hide:Relate:NARUDZBENICA &= NEW(Hide:Relate:NARUDZBENICA)
  _Hide:Access:MJESTO &= NEW(Hide:Access:MJESTO)
  _Hide:Relate:MJESTO &= NEW(Hide:Relate:MJESTO)
  _Hide:Access:IZDAVACKA_KUCA &= NEW(Hide:Access:IZDAVACKA_KUCA)
  _Hide:Relate:IZDAVACKA_KUCA &= NEW(Hide:Relate:IZDAVACKA_KUCA)
  _Hide:Access:NARUCITELJ &= NEW(Hide:Access:NARUCITELJ)
  _Hide:Relate:NARUCITELJ &= NEW(Hide:Relate:NARUCITELJ)
  _Hide:Access:NACIN_DOSTAVE &= NEW(Hide:Access:NACIN_DOSTAVE)
  _Hide:Relate:NACIN_DOSTAVE &= NEW(Hide:Relate:NACIN_DOSTAVE)
  _Hide:Access:JEDINICA_MJERE &= NEW(Hide:Access:JEDINICA_MJERE)
  _Hide:Relate:JEDINICA_MJERE &= NEW(Hide:Relate:JEDINICA_MJERE)
  _Hide:Access:ARTIKL &= NEW(Hide:Access:ARTIKL)
  _Hide:Relate:ARTIKL &= NEW(Hide:Relate:ARTIKL)
  _Hide:Access:STAVKA &= NEW(Hide:Access:STAVKA)
  _Hide:Relate:STAVKA &= NEW(Hide:Relate:STAVKA)
  Relate:NARUDZBENICA &= _Hide:Relate:NARUDZBENICA
  Relate:MJESTO &= _Hide:Relate:MJESTO
  Relate:IZDAVACKA_KUCA &= _Hide:Relate:IZDAVACKA_KUCA
  Relate:NARUCITELJ &= _Hide:Relate:NARUCITELJ
  Relate:NACIN_DOSTAVE &= _Hide:Relate:NACIN_DOSTAVE
  Relate:JEDINICA_MJERE &= _Hide:Relate:JEDINICA_MJERE
  Relate:ARTIKL &= _Hide:Relate:ARTIKL
  Relate:STAVKA &= _Hide:Relate:STAVKA


ZavrsniML_BC0:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:NARUDZBENICA.Init
  _Hide:Relate:MJESTO.Init
  _Hide:Relate:IZDAVACKA_KUCA.Init
  _Hide:Relate:NARUCITELJ.Init
  _Hide:Relate:NACIN_DOSTAVE.Init
  _Hide:Relate:JEDINICA_MJERE.Init
  _Hide:Relate:ARTIKL.Init
  _Hide:Relate:STAVKA.Init


ZavrsniML_BC0:DctKill PROCEDURE()
  CODE
  _Hide:Relate:NARUDZBENICA.Kill
  DISPOSE(_Hide:Relate:NARUDZBENICA)
  _Hide:Relate:MJESTO.Kill
  DISPOSE(_Hide:Relate:MJESTO)
  _Hide:Relate:IZDAVACKA_KUCA.Kill
  DISPOSE(_Hide:Relate:IZDAVACKA_KUCA)
  _Hide:Relate:NARUCITELJ.Kill
  DISPOSE(_Hide:Relate:NARUCITELJ)
  _Hide:Relate:NACIN_DOSTAVE.Kill
  DISPOSE(_Hide:Relate:NACIN_DOSTAVE)
  _Hide:Relate:JEDINICA_MJERE.Kill
  DISPOSE(_Hide:Relate:JEDINICA_MJERE)
  _Hide:Relate:ARTIKL.Kill
  DISPOSE(_Hide:Relate:ARTIKL)
  _Hide:Relate:STAVKA.Kill
  DISPOSE(_Hide:Relate:STAVKA)


Hide:Relate:NARUDZBENICA.Init PROCEDURE()
  CODE
  _Hide:Access:NARUDZBENICA.Init
  SELF.Init(Access:NARUDZBENICA,1)


Hide:Access:NARUDZBENICA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= NAR:Record
  SELF.FileNameValue = 'NARUDZBENICA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= NARUDZBENICA
  PARENT.Init
  Access:NARUDZBENICA &= SELF


Hide:Access:NARUDZBENICA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:NARUDZBENICA &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:NARUDZBENICA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(NARUDZBENICA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(NAR:PK_Narudzbenica,'NAR:PK_Narudzbenica',0)
    SELF.AddKey(NAR:VK_NarudzbenicaOIB,'NAR:VK_NarudzbenicaOIB',0)
    SELF.AddKey(NAR:VK_NarudzbenicaIK,'NAR:VK_NarudzbenicaIK',0)
    SELF.AddKey(NAR:VK_NarudzbenicaND,'NAR:VK_NarudzbenicaND',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:NARUDZBENICA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:NARUCITELJ)

  SELF.AddRelation(Relate:IZDAVACKA_KUCA)

  SELF.AddRelation(Relate:NACIN_DOSTAVE)

  SELF.AddRelation(Relate:STAVKA,RI:CASCADE,RI:CASCADE,STA:PK_Stavka)
  SELF.AddRelationLink(NAR:Broj_narudzbe,STA:Broj_narudzbe)


Hide:Relate:NARUDZBENICA.Kill PROCEDURE

  CODE
  _Hide:Access:NARUDZBENICA.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:NARUDZBENICA &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:NARUDZBENICA)                       ! destroy the file manager


Hide:Relate:MJESTO.Init PROCEDURE()
  CODE
  _Hide:Access:MJESTO.Init
  SELF.Init(Access:MJESTO,1)


Hide:Access:MJESTO.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= MJE:Record
  SELF.FileNameValue = 'MJESTO'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= MJESTO
  PARENT.Init
  Access:MJESTO &= SELF


Hide:Access:MJESTO.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:MJESTO &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:MJESTO.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(MJESTO,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(MJE:PK_Mjesto,'MJE:PK_Mjesto',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:MJESTO.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:IZDAVACKA_KUCA,RI:CASCADE,RI:RESTRICT,IZD:VK_IzdavackaKuca)
  SELF.AddRelationLink(MJE:Postanski_broj,IZD:Postanski_broj)

  SELF.AddRelation(Relate:NARUCITELJ,RI:CASCADE,RI:RESTRICT,NARU:VK_Narucitelj)
  SELF.AddRelationLink(MJE:Postanski_broj,NARU:Postanski_broj)


Hide:Relate:MJESTO.Kill PROCEDURE

  CODE
  _Hide:Access:MJESTO.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:MJESTO &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:MJESTO)                             ! destroy the file manager


Hide:Relate:IZDAVACKA_KUCA.Init PROCEDURE()
  CODE
  _Hide:Access:IZDAVACKA_KUCA.Init
  SELF.Init(Access:IZDAVACKA_KUCA,1)


Hide:Access:IZDAVACKA_KUCA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= IZD:Record
  SELF.FileNameValue = 'IZDAVACKA_KUCA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= IZDAVACKA_KUCA
  PARENT.Init
  Access:IZDAVACKA_KUCA &= SELF


Hide:Access:IZDAVACKA_KUCA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:IZDAVACKA_KUCA &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:IZDAVACKA_KUCA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(IZDAVACKA_KUCA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(IZD:PK_IzdavackaKuca,'IZD:PK_IzdavackaKuca',0)
    SELF.AddKey(IZD:VK_IzdavackaKuca,'IZD:VK_IzdavackaKuca',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:IZDAVACKA_KUCA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:NARUDZBENICA,RI:CASCADE,RI:RESTRICT,NAR:VK_NarudzbenicaIK)
  SELF.AddRelationLink(IZD:Sifra_izdavacke_kuce,NAR:Sifra_izdavacke_kuce)

  SELF.AddRelation(Relate:MJESTO)


Hide:Relate:IZDAVACKA_KUCA.Kill PROCEDURE

  CODE
  _Hide:Access:IZDAVACKA_KUCA.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:IZDAVACKA_KUCA &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:IZDAVACKA_KUCA)                     ! destroy the file manager


Hide:Relate:NARUCITELJ.Init PROCEDURE()
  CODE
  _Hide:Access:NARUCITELJ.Init
  SELF.Init(Access:NARUCITELJ,1)


Hide:Access:NARUCITELJ.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= NARU:Record
  SELF.FileNameValue = 'NARUCITELJ'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= NARUCITELJ
  PARENT.Init
  Access:NARUCITELJ &= SELF


Hide:Access:NARUCITELJ.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:NARUCITELJ &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:NARUCITELJ.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(NARUCITELJ,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(NARU:PK_Narucitelj,'NARU:PK_Narucitelj',0)
    SELF.AddKey(NARU:VK_Narucitelj,'NARU:VK_Narucitelj',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:NARUCITELJ.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:NARUDZBENICA,RI:CASCADE,RI:RESTRICT,NAR:VK_NarudzbenicaOIB)
  SELF.AddRelationLink(NARU:OIB_narucitelja,NAR:OIB_narucitelja)

  SELF.AddRelation(Relate:MJESTO)


Hide:Relate:NARUCITELJ.Kill PROCEDURE

  CODE
  _Hide:Access:NARUCITELJ.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:NARUCITELJ &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:NARUCITELJ)                         ! destroy the file manager


Hide:Relate:NACIN_DOSTAVE.Init PROCEDURE()
  CODE
  _Hide:Access:NACIN_DOSTAVE.Init
  SELF.Init(Access:NACIN_DOSTAVE,1)


Hide:Access:NACIN_DOSTAVE.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= NAC:Record
  SELF.FileNameValue = 'NACIN_DOSTAVE'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= NACIN_DOSTAVE
  PARENT.Init
  Access:NACIN_DOSTAVE &= SELF


Hide:Access:NACIN_DOSTAVE.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:NACIN_DOSTAVE &= NULL                             ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:NACIN_DOSTAVE.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(NACIN_DOSTAVE,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(NAC:PK_NacinDostave,'NAC:PK_NacinDostave',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:NACIN_DOSTAVE.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 2
    GlobalErrors.SetField('NAC:Naziv_nacina_dostave')
    IF INSTRING(']' & NAC:Naziv_nacina_dostave & '[', ']Standardna_dostava[]Premium_dostava[]Brza_dostava[]Besplatna_dostava[', 1, 1) = 0
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInList,'''STANDARDNO'',''PREMIUM'',''BRZO'',''BESPLATNO''')
      END
    END
  END
  RETURN ReturnValue


Hide:Relate:NACIN_DOSTAVE.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:NARUDZBENICA,RI:CASCADE,RI:RESTRICT,NAR:VK_NarudzbenicaND)
  SELF.AddRelationLink(NAC:Sifra_nacina_dostave,NAR:Sifra_nacina_dostave)


Hide:Relate:NACIN_DOSTAVE.Kill PROCEDURE

  CODE
  _Hide:Access:NACIN_DOSTAVE.Kill                          ! Kill the file manager
  PARENT.Kill
  Relate:NACIN_DOSTAVE &= NULL                             ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:NACIN_DOSTAVE)                      ! destroy the file manager


Hide:Relate:JEDINICA_MJERE.Init PROCEDURE()
  CODE
  _Hide:Access:JEDINICA_MJERE.Init
  SELF.Init(Access:JEDINICA_MJERE,1)


Hide:Access:JEDINICA_MJERE.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= JED:Record
  SELF.FileNameValue = 'JEDINICA_MJERE'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= JEDINICA_MJERE
  PARENT.Init
  Access:JEDINICA_MJERE &= SELF


Hide:Access:JEDINICA_MJERE.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:JEDINICA_MJERE &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:JEDINICA_MJERE.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(JEDINICA_MJERE,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(JED:PK_JedinicaMjere,'JED:PK_JedinicaMjere',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:JEDINICA_MJERE.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:ARTIKL,RI:CASCADE,RI:RESTRICT,ART:VK_Artikl)
  SELF.AddRelationLink(JED:Sifra_jedinice_mjere,ART:Sifra_jedinice_mjere)


Hide:Relate:JEDINICA_MJERE.Kill PROCEDURE

  CODE
  _Hide:Access:JEDINICA_MJERE.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:JEDINICA_MJERE &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:JEDINICA_MJERE)                     ! destroy the file manager


Hide:Relate:ARTIKL.Init PROCEDURE()
  CODE
  _Hide:Access:ARTIKL.Init
  SELF.Init(Access:ARTIKL,1)


Hide:Access:ARTIKL.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= ART:Record
  SELF.FileNameValue = 'ARTIKL'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= ARTIKL
  PARENT.Init
  Access:ARTIKL &= SELF


Hide:Access:ARTIKL.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:ARTIKL &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:ARTIKL.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(ARTIKL,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(ART:PK_Artikl,'ART:PK_Artikl',0)
    SELF.AddKey(ART:VK_Artikl,'ART:VK_Artikl',0)
    SELF.AddKey(ART:SK_ArtiklNaziv,'ART:SK_ArtiklNaziv',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:ARTIKL.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:JEDINICA_MJERE)

  SELF.AddRelation(Relate:STAVKA,RI:CASCADE,RI:RESTRICT,STA:VK_Stavka)
  SELF.AddRelationLink(ART:Sifra_artikla,STA:Sifra_artikla)


Hide:Relate:ARTIKL.Kill PROCEDURE

  CODE
  _Hide:Access:ARTIKL.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:ARTIKL &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:ARTIKL)                             ! destroy the file manager


Hide:Relate:STAVKA.Init PROCEDURE()
  CODE
  _Hide:Access:STAVKA.Init
  SELF.Init(Access:STAVKA,1)


Hide:Access:STAVKA.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= STA:Record
  SELF.FileNameValue = 'STAVKA'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= STAVKA
  PARENT.Init
  Access:STAVKA &= SELF


Hide:Access:STAVKA.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:STAVKA &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:STAVKA.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(STAVKA,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(STA:PK_Stavka,'STA:PK_Stavka',0)
    SELF.AddKey(STA:VK_Stavka,'STA:VK_Stavka',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:STAVKA.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:ARTIKL)

  SELF.AddRelation(Relate:NARUDZBENICA)


Hide:Relate:STAVKA.Kill PROCEDURE

  CODE
  _Hide:Access:STAVKA.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:STAVKA &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:STAVKA)                             ! destroy the file manager

