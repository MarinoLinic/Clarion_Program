  MEMBER('ZavrsniML.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('ZavrsniML_BC0.CLW')
ZavrsniML_BC0:DctInit             PROCEDURE()
ZavrsniML_BC0:DctKill             PROCEDURE()
ZavrsniML_BC0:FilesInit           PROCEDURE()
    END
  END

DctInit PROCEDURE()
  CODE
  ZavrsniML_BC0:DctInit
  ZavrsniML_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  ZavrsniML_BC0:DctKill

