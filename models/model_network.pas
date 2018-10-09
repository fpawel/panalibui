unit model_network;

interface

type TNetworkPlace = class
    FAddr : integer;
    FUnchecked : boolean;
end;

type TDeviceVar = class
    FVar : integer;
    FName : string;
    FUnchecked : boolean;
end;

type TReadVar = class
    FPlace : integer;
    FVarIndex : integer;
    FValue: double;
    FError: string;
end;


(*
struct {
				Place, VarNumber int
				Value            float64
				Error            string
			}


type Place struct {
	Addr      modbus.Addr `json:"addr"`
	Unchecked bool        `json:",omitempty"`
}

type Var struct {
	Var       modbus.Var `json:"var"`
	Name      string     `json:"name"`
	Unchecked bool       `json:"unchecked,omitempty"`
}
*)

implementation

end.
