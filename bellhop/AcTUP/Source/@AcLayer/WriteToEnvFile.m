function WriteToEnvFile(Obj, Freq, Cpmax, EnvFile, ZTop)
%Freq = frequency (Hz)
%Cpmax = maximum horizontal phase speed (m/s)
%EnvFile = pointer to output file (must already be open)
%ZTop = depth of top of layer below water surface (m)

if Obj.IsHalfSpace
   BottomStr = '''A''';  %A = acousto-elastic half-space
   fprintf(EnvFile, '%s  %f\n', BottomStr, Obj.RMSRough);
   fprintf(EnvFile, '%f  %f  %f  %f  %f  %f\n', ...
      Obj.Z(1)+ZTop, Obj.Cp(1), Obj.Cs(1), Obj.Rho(1)/1000, Obj.Ap(1), Obj.As(1));
else
   %Calculate the number of vertical meshpoints required (40 per vertical wavelength)
   %Also place a lower limit of 20 points for each layer as otherwise problems can occur at
   %low frequencies.
   if max(Obj.Cs > 10)
      Cmin = min(Obj.Cs);
   else
      Cmin = min(Obj.Cp);
   end
   
	%LambdaVert = Cmin / (Freq * sqrt(1 - (Cmin/Cpmax)^2));  % Minimum vetical wavelength
	%LambdaVert = Cmin / Freq;  %Minimum wavelength
	%NLambda = max(Obj.Z) / LambdaVert;
   %NMesh = ceil(40*NLambda);
   %if NMesh < 20
   %   NMesh = 20;
   %end
   %Changed 11/6/02
   %New versions of AcToolbox programs automatically calculate the mesh size if the 
   %number of mesh points is set to zero
   NMesh = 0;

	fprintf(EnvFile, '%d  %f  %f\n', NMesh, Obj.RMSRough, Obj.Z(end)+ZTop);
   for ISub = 1:length(Obj.Z)
      fprintf(EnvFile, '%f  %f  %f  %f  %f  %f\n', ...
         Obj.Z(ISub)+ZTop, Obj.Cp(ISub), Obj.Cs(ISub), Obj.Rho(ISub)/1000, Obj.Ap(ISub), Obj.As(ISub));
   end
end