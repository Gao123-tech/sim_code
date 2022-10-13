function PlotAcEnvArr(EnvArrObj)
%PlotAcEnvArr           Menu driven function to plot the environment information
%
%USAGE                  PlotAcEnvArr(EnvArrObj)
%
%Revision 0.0   ?? ??????? ???? ... AJD
%
%Revision 0.1   25 August  2006 ... ALM
%               - see comments in PlotAcEnvironment
%               - grab expansion of halfspace layer (from single point to N points) 
%                 that AJD used for pseudo colour plot and apply to line plot

NEl        = GetNumberOfElements(EnvArrObj);
% MOD v0.1 - this is now more global (not plot dependent)
HalfspaceZ = 100;  %Plot halfspace this thick 

if NEl <= 1
    PlotEnvironment(EnvArrObj.EnvArr{1});
else
    Done = 0;
    MenuTxt = {'Plot Cp', 'Plot Cs', 'Plot Density', 'Plot Ap', 'Plot As', 'Done'};
    AllPlotType = {'Pseudo colour',  'Line'};
    AllPlotRegion = {'Water column', 'Seabed', 'Both'};
    Style = {'b', 'r', 'y', 'g', 'm', 'c', 'k', 'b:', 'r:', 'y:', 'g:', 'm:', 'c:', 'k:', ...
            'b--', 'r--', 'y--', 'g--', 'm--', 'c--', 'k--', 'b-.', 'r-.', 'y-.', 'g-.', 'm-.', 'c-.', 'k-.'};
    NStyle = length(Style);
    
    State = 'Menu';
    while ~Done
        switch State
            case 'Menu'
                MenuOpt = menu('Environment array plotting options', MenuTxt);
                State = MenuTxt{MenuOpt};
                
            case {'Plot Cp', 'Plot Cs', 'Plot Density', 'Plot Ap', 'Plot As'}
               % ALM - grab next figure number ---------------------
               [fhdls, currentfig, nextfig, unusedfig] = FindFigures();
               % ALM -----------------------------------------------
                Ans = inputdlg({'Figure number', 'Interpolate data before plotting (y/n)' }, '', 1, {num2str(nextfig), 'n'});
                
                if ~isempty(Ans)
                    FigNum = str2num(Ans{1});
                    DoInterpolate = strcmpi(Ans{2}, 'y');
                    PlotOpt = menu('Plot type:', AllPlotType);
                    PlotType = AllPlotType{PlotOpt};
                    
                    RegionOpt = menu('Plot data for:', AllPlotRegion);
                    PlotRegion = AllPlotRegion{RegionOpt};
                    
                    if DoInterpolate
                        InterpObj = Interpolate(EnvArrObj);
                    else
                        InterpObj = EnvArrObj;                    
                    end
                    RVec = InterpObj.RangeVec;
                    NEnv = length(RVec);
                    EnvArr = InterpObj.EnvArr;
                    
                    switch State
                        case 'Plot Cp', 
                            DatType = 'Cp';
                            Label = 'Compressional sound speed (m/s)';
                        case 'Plot Cs', 
                            DatType = 'Cs';
                            Label = 'Shear sound speed (m/s)';
                        case 'Plot Density', 
                            DatType = 'Rho';
                            Label = 'Density (kg.m^-^3)';
                        case 'Plot Ap', 
                            DatType = 'Ap';
                            Label = 'Compressional attenuation (dB/wavelength)';
                        case 'Plot As'
                            DatType = 'As';
                            Label = 'Shear attenuation (dB/wavelength)';
                    end
                    
                    switch PlotType
                        case 'Pseudo colour',  
                            NZ = 500;  %Number of points to plot in Z direction
                            %First determine the limits of the plotting region
                            ZMax = 0;
                            for IEnv = 1:NEnv
                                switch PlotRegion
                                    case 'Water column', 
                                        ZWater = GetWaterDepth(EnvArr{IEnv});
                                        if ZMax < ZWater
                                            ZMax = ZWater;
                                        end
                                        
                                    case 'Seabed'
                                        [ZSub, IsHalfspace] = GetSubstrateThickness(EnvArr{IEnv});
                                        if IsHalfspace
                                            ZSub = ZSub + HalfspaceZ;
                                        end
                                        if ZMax < ZSub
                                            ZMax = ZSub;
                                        end
                                        
                                    case 'Both'
                                        ZWater = GetWaterDepth(EnvArr{IEnv});
                                        [ZSub, IsHalfspace] = GetSubstrateThickness(EnvArr{IEnv});
                                        Z = ZWater + ZSub;
                                        if IsHalfspace
                                            Z = Z + HalfspaceZ;
                                        end
                                        if ZMax < Z
                                            ZMax = Z;
                                        end
                                end
                            end
                            if ZMax == 0   %This can occur for a halfspace seabed
                                ZMax = 100;
                            end
                            dZ = ZMax / (NZ - 1);
                            ZVec = [0:dZ:ZMax];  
                            NZ = length(ZVec);
                            PltArr = zeros(NZ, NEnv);
                            
                            
                            for IEnv = 1:NEnv
                                NLayer = GetNumLayers(EnvArr{IEnv});
                                Data = [];
                                Z = [];
                                ZOff = 0;
                                switch PlotRegion
                                    case {'Water column', 'Both'}
                                        Layer = GetLayer(EnvArr{IEnv}, 1);
                                        Profile = GetProfile(Layer, DatType, 1);
                                        Z = [Z, ZOff + Profile(1, :)];
                                        Data = [Data, Profile(2, :)];
                                        ZOff = Z(end)+1e-5;  %Small ofset to prevent interpolation problems
                                end
                                
                                switch PlotRegion
                                    case {'Seabed', 'Both'}
                                        for ILayer = 2:NLayer
                                            Layer = GetLayer(EnvArr{IEnv}, ILayer);
                                            Profile = GetProfile(Layer, DatType, 1);
                                            Z = [Z, ZOff + Profile(1, :)];
                                            Data = [Data, Profile(2, :)];
                                            if IsHalfSpace(Layer)
                                                Data = [Data, Data(end)];
                                                Z = [Z, Z(end)+HalfspaceZ];
                                            end
                                            ZOff = Z(end)+1e-5;  %Small ofset to prevent interpolation problems
                                        end
                                end
                                switch PlotRegion
                                    case 'Water column'
                                        ExtrapVal = nan;
                                    case {'Seabed', 'Both'}
                                        ExtrapVal = Data(end);
                                end
                                PltArr(:, IEnv) = interp1(Z, Data, ZVec, 'linear', ExtrapVal).';
                            end
                            figure(FigNum);
                            GUI_SetupGraphMenu('pcolor');
                            pcolor(RVec, ZVec, PltArr);
                            shading interp;
                            GUI_Colorbar(Label);
                            xlabel('Range (m)');
                            ylabel('Depth (m)');
                            view(0, -90);
                            
                            
                            
                        case 'Line'
                            figure(FigNum);
                            GUI_SetupGraphMenu('plot');
                            hold on;
                            
                            for IEnv = 1:NEnv
                                NLayer = GetNumLayers(EnvArr{IEnv});
                                Data = [];
                                Z = [];
                                ZOff = 0;
                                switch PlotRegion
                                    case {'Water column', 'Both'}
                                        Layer = GetLayer(EnvArr{IEnv}, 1);
                                        Profile = GetProfile(Layer, DatType, 1);
                                        if IsHalfSpace(Layer)
                                           Zadd = [ZOff, ZOff+HalfspaceZ] + Profile(1, :);
                                           Z    = [Z, Zadd];
                                           Data = [Data, ones(size(Zadd)).* Profile(2, :)];
                                        else
                                           Z = [Z, ZOff + Profile(1, :)];
                                           Data = [Data, Profile(2, :)];
                                        end
                                        ZOff = Z(end)+1e-5;  %Small ofset to prevent interpolation problems
                                end
                                
                                switch PlotRegion
                                    case {'Seabed', 'Both'}
                                        for ILayer = 2:NLayer
                                            Layer = GetLayer(EnvArr{IEnv}, ILayer);
                                            Profile = GetProfile(Layer, DatType, 1);
                                            if IsHalfSpace(Layer)
                                               Zadd = [ZOff, ZOff+HalfspaceZ] + Profile(1, :);
                                               Z    = [Z, Zadd];
                                               Data = [Data, ones(size(Zadd)).* Profile(2, :)];
                                            else
                                               Z = [Z, ZOff + Profile(1, :)];
                                               Data = [Data, Profile(2, :)];
                                            end
                                            ZOff = Z(end)+1e-5;  %Small ofset to prevent interpolation problems
                                        end
                                end
                                
                                IStyle = mod((IEnv-1), NStyle) + 1;
                                plot(Data, Z, Style{IStyle});
                                xlabel(Label)
                                ylabel('Depth (m)');
                                view(0, -90);
                            end
                            
                    end
                end
                State = 'Menu';
                
            case 'Done'
                Done = 1;
        end
        
    end
end