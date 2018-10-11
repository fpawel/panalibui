unit serverapp_msg;

interface

type
    TServerAppUserMsg = (msgPeer, msgSetVarChecked, msgSetPlaceChecked,
      msgAddDelVar, msgAddDelPlace, msgSetAddr, msgSetVar, msgToggle);

    TServerAppDataMsg = (dmsgSetsProp, dmsgWrite16);

implementation

end.
