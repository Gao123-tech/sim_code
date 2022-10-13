function Str = Obj2Str(Obj)
%Converts the object to a one dimensional cell array of strings
%disp('Source - Obj2Str');
S = struct(Obj);

Str = Struct2Str(S);