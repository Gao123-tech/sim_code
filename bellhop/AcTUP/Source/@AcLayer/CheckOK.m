function [Str, Obj] = CheckOK(Obj);
%Checks whether the object parameters are valid
%Also puts the vector parameters into a standard format.

Str = 'OK';
if Obj.IsHalfSpace
   if (length(Obj.Z) ~= 1) | (length(Obj.Cp) ~= 1) | (length(Obj.Rho) ~= 1) 
      Str = 'Z, Cp and Rho Vectors must be length 1 for a halfspace layer';
   else
      if (length(Obj.Cs) ~= 1)
         Obj.Cs = 0;
      end
      if (length(Obj.As) ~= 1)
         Obj.As = 0;
      end
      if (length(Obj.Ap) ~= 1)
         Obj.Ap = 0;
      end
   end
else
  if length(Obj.Z) <= 1
     Str = 'Z vector must be of length > 1';
  else
     NZ = length(Obj.Z);
     [Str, Obj.Cp] = lCheckVec(Obj.Cp, 'Cp', NZ, []);
     if strcmp(Str, 'OK')
      	[Str, Obj.Rho] = lCheckVec(Obj.Rho, 'Rho', NZ, []);
     end
         
     if strcmp(Str, 'OK')
      	[Str, Obj.Cs] = lCheckVec(Obj.Cs, 'Cs', NZ, 0);
     end
         
     if strcmp(Str, 'OK')
       	[Str, Obj.Ap] = lCheckVec(Obj.Ap, 'Ap', NZ, 0);
     end
         
     if strcmp(Str, 'OK')
        	[Str, Obj.As] = lCheckVec(Obj.As, 'As', NZ, 0);
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Str, Vec] = lCheckVec(Vec, Name, NZ, Default);
switch(length(Vec))
case 0,
   if isempty(Default)
      Str = ['Empty ' Name ' vector not allowed'];
   else
      Vec(1, 1:NZ) = Default;
		Str = 'OK';
   end
   
case 1,
   Vec(1, 1:NZ) = Vec(1);
	Str = 'OK';
            
case NZ,
	Str = 'OK';
otherwise,
   if isempty(Default)
      Str = ['Vector ' Name ' must be length 1 or same length as Z'];
   else
      Str = ['Vector ' Name ' must be length 0, 1 or same length as Z'];
   end
end

      
            
         
         