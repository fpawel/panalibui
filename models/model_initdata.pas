unit model_initdata;

interface

uses model_config, model_network;

type TInitData = class
    FConfig : TConfig;
    FVars : TArray<TDeviceVar>;
    FNetwork : TArray<TNetworkPlace>;
end;

implementation

end.
