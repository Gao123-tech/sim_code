function varargout = menumaster(varargin)
% menumaster M-file for menumaster.fig
%      menumaster, by itself, creates a new menumaster or raises the existing
%      singleton*.
%
%      H = menumaster returns the handle to a new menumaster or the handle to
%      the existing singleton*.
%
%      menumaster('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in menumaster.M with the given input arguments.
%
%      menumaster('Property','Value',...) creates a new menumaster or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the menumaster before menumaster_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to menumaster_OpeningFcn via varargin.
%
%      *See menumaster Options on GUIDE's Tools menu.  Choose "menumaster allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help menumaster

% Last Modified by GUIDE v2.5 21-Mar-2005 11:11:38

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @menumaster_OpeningFcn, ...
    'gui_OutputFcn',  @menumaster_OutputFcn, ...
    'gui_LayoutFcn',  [], ...
    'gui_Callback',   []);
if nargin && isstr(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before menumaster is made visible.
function menumaster_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to menumaster (see VARARGIN)

% Choose default command line output for menumaster
% following specifies location of help files

handles.atdoc = 'C:\Documents and Settings\michael b. porter\My Documents\at\doc\';
disp( 'Path for documentation files:' );
handles.atdoc

handles.output = hObject;
handles.root   = '';
handles.envfil = '';
handles.atifil = '';
handles.btyfil = '';
handles.brcfil = '';
handles.trcfil = '';
handles.sbpfil = '';

handles.modfil = '';
handles.rayfil = '';
handles.grnfil = '';
handles.shdfil = '';
handles.matfil = '';    % assumed to contain TL data if exists
handles.prtfil = '';

if ( exist( 'FIELD.FLP' ) == 2 )
    set( [handles.editFIELD] ,'enable', 'on' );
end
if ( exist( 'FIELDS.FLP' ) == 2 )
    set( [handles.editFIELDS] ,'enable', 'on' );
end

CheckPlotFiles( handles );  % enable buttons to plot files that exist

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes menumaster wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = menumaster_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

% --- If Enable == 'on', executes on mouse press in 5 pixel border.
% --- Otherwise, executes on mouse press in 5 pixel border or over BELLHOP.
function BELLHOP_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to BELLHOP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in BELLHOP.
function BELLHOP_Callback(hObject, eventdata, handles)
% hObject    handle to BELLHOP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BELLHOP

bellhop( handles.root );
set( hObject, 'Value', 0.0 );   % pop button up
CheckPlotFiles( handles );

% --- Executes during object creation, after setting all properties.
function popupmenu1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

% --------------------------------------------------------------------
function KRAKEN_Callback(hObject, eventdata, handles)
% hObject    handle to KRAKEN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

kraken( handles.root ); 
set( hObject, 'Value', 0.0 );   % pop button up
CheckPlotFiles( handles );

% --------------------------------------------------------------------
function SCOOTER_Callback(hObject, eventdata, handles)
% hObject    handle to SCOOTER (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

scooter( handles.root );
set( hObject, 'Value', 0.0 );   % pop button up
CheckPlotFiles( handles );


% --------------------------------------------------------------------
function KRAKENC_Callback(hObject, eventdata, handles)
% hObject    handle to KRAKENC (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

krakenc( handles.root )

set( hObject, 'Value', 0.0 );   % pop button up
CheckPlotFiles( handles );


% --------------------------------------------------------------------
function Select_Callback(hObject, eventdata, handles)
% hObject    handle to Select (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

d = dir( '*.env' );
if isempty( d )
    warndlg( 'No envfil in this directory', 'Warning' );
else
    filenames = {d.name};
    [ s, v ] = listdlg( 'PromptString', 'Select a file:',...
        'SelectionMode', 'single',...
        'ListString', filenames );
    handles.envfil = char( filenames( s ) );
    handles.root = handles.envfil( 1:length( handles.envfil ) - 4 );
    handles.atifil = [ handles.root '.ati' ];
    handles.btyfil = [ handles.root '.bty' ];
    handles.trcfil = [ handles.root '.trc' ];
    handles.brcfil = [ handles.root '.brc' ];
    handles.sbpfil = [ handles.root '.sbp' ];
    handles.modfil = [ handles.root '.mod' ];
    handles.rayfil = [ handles.root '.ray' ];
    handles.grnfil = [ handles.root '.grn' ];
    handles.shdfil = [ handles.root '.shd' ];
    handles.prtfil = [ handles.root '.prt' ];
    handles.matfil = [ handles.root '.mat' ];   % assumes a mat file contains TL data
    
    set( [handles.editENVFIL] ,'enable', 'on' );
    
    if ( exist( handles.atifil ) == 2 )
        set( [handles.editATIFIL], 'enable', 'on' );
        set( [handles.PLOTATI]   , 'enable', 'on' );
    else
        set( [handles.editATIFIL], 'enable', 'off' );
        set( [handles.PLOTATI]   , 'enable', 'off' );
    end
    if ( exist( handles.btyfil ) == 2 )
        set( [handles.editBTYFIL], 'enable', 'on' );
        set( [handles.PLOTBTY],    'enable', 'on' );
    else
        set( [handles.editBTYFIL], 'enable', 'off' );
        set( [handles.PLOTBTY],    'enable', 'off' );
    end
    
    set( [handles.BELLHOP], 'enable', 'on' );
    set( [handles.BOUNCE] , 'enable', 'on' );
    set( [handles.KRAKEN] , 'enable', 'on' );
    set( [handles.KRAKENC], 'enable', 'on' );
    set( [handles.SCOOTER], 'enable', 'on' );
    set( [handles.SPARC]  , 'enable', 'on' );
    set( hObject, 'Value', 1.0 );   % show pushed
end

CheckPlotFiles( handles );

% Update handles structure
guidata(hObject, handles);


% --------------------------------------------------------------------
function BOUNCE_Callback(hObject, eventdata, handles)
% hObject    handle to BOUNCE (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


bounce( handles.root );
set( hObject, 'Value', 0.0 );   % pop button up
CheckPlotFiles( handles );


% --------------------------------------------------------------------
function PLOTRAY_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTRAY (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
plotray( handles.root ); 

% --------------------------------------------------------------------
function PLOTSHD_Callback(hObject, eventdata, handles)
% hObject    handle to plotshd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
if ( exist( [ handles.root '.mat' ] ) == 2 )
   plotshd( [ handles.root '.mat' ] );
else
   plotshd( [ handles.root '.shd' ] );  
end

FigNum = str2double( get( handles.FigNum, 'String' ) );
caxisdat = eval( get( handles.caxis_vec, 'String' ) );
caxis( caxisdat ); figure( FigNum ); colorbar( 'horiz' )

% --------------------------------------------------------------------
function PLOTBTY_Callback(hObject, eventdata, handles)
% hObject    handle to Bathymetry (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
plotbty( handles.root )

% --------------------------------------------------------------------
function PLOTATI_Callback(hObject, eventdata, handles)
% hObject    handle to Altimetry (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
plotati( handles.root )

% --------------------------------------------------------------------
function PLOTRTH_Callback(hObject, eventdata, handles)
% hObject    handle to Refco (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
plotrth( handles.root )

% --------------------------------------------------------------------
function PLOTMODE_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTMODE (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
plotmode( handles.root )

% --------------------------------------------------------------------
function NewFigure_Callback(hObject, eventdata, handles)
% hObject    handle to NewFigure (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

figure


% ------------------------------------------------------------
% Callback for list box - open .fig with guide, otherwise use open
% ------------------------------------------------------------
function varargout = listbox1_Callback(h, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox1

get(handles.figure1,'SelectionType');
if strcmp(get(handles.figure1,'SelectionType'),'open')
    index_selected = get(handles.listbox1,'Value');
    file_list = get(handles.listbox1,'String');	
    filename = file_list{index_selected};
    if  handles.is_dir(handles.sorted_index(index_selected))
        cd (filename)
        load_listbox(pwd,handles)
    else
        [path,name,ext,ver] = fileparts(filename);
        switch ext
            case '.fig'
                guide (filename)
            otherwise 
                try
                    open(filename)
                catch
                    errordlg(lasterr,'File Type Error','modal')
                end
        end	
    end
end
% ------------------------------------------------------------
% Read the current directory and sort the names
% ------------------------------------------------------------
function load_listbox( dir_path, handles )
cd (dir_path)
dir_struct = dir( dir_path );
[sorted_names,sorted_index] = sortrows({dir_struct.name}');
handles.file_names = sorted_names;
handles.is_dir = [dir_struct.isdir];
handles.sorted_index = [sorted_index];
guidata(handles.figure1,handles)
set(handles.listbox1,'String',handles.file_names,...
    'Value',1)
set(handles.text1,'String',pwd)


% --- Executes during object creation, after setting all properties.
function listbox1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background, change
%       'usewhitebg' to 0 to use default.  See ISPC and COMPUTER.
usewhitebg = 1;
if usewhitebg
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

% --- Executes during object creation, after setting all properties.
function listbox3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

% --------------------------------------------------------------------
function editFIELDS_Callback(hObject, eventdata, handles)
% hObject    handle to EditFIELDS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( '!notepad fields.flp' );

% --------------------------------------------------------------------
function PLOTTLR_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTTLR (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
rdt = str2double( get( handles.rdt, 'String' ) );
plottlr( handles.root, rdt )

% --------------------------------------------------------------------
function PLOTTLD_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTTLD (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

MakeFig( handles )
rrt = str2double( get( handles.rrt, 'String' ) );
plottld( handles.root, rrt )

% --- Executes during object creation, after setting all properties.
function rdt_CreateFcn(hObject, eventdata, handles)
% hObject    handle to rdt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function rdt_Callback(hObject, eventdata, handles)
% hObject    handle to rdt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of rdt as text
%        str2double(get(hObject,'String')) returns contents of rdt as a double

% --- Executes during object creation, after setting all properties.
function caxis_vec_CreateFcn(hObject, eventdata, handles)
% hObject    handle to caxis_vec (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function caxis_vec_Callback(hObject, eventdata, handles)
% hObject    handle to caxis_vec (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of caxis_vec as text
%        str2double(get(hObject,'String')) returns contents of caxis_vec as a double

% --- Executes on button press in SPARC.
function SPARC_Callback(hObject, eventdata, handles)
% hObject    handle to SPARC (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of SPARC

% --- Executes on button press in Overlay.
function Overlay_Callback(hObject, eventdata, handles)
% hObject    handle to Overlay (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of Overlay

function CheckPlotFiles( handles )

if ( exist( handles.modfil ) == 2 )
    set( [handles.PLOTMODE], 'enable', 'on' );
else
    set( [handles.PLOTMODE], 'enable', 'off' );
end

if ( exist( handles.rayfil ) == 2 )
    set( [handles.PLOTRAY], 'enable', 'on' );
else
    set( [handles.PLOTRAY], 'enable', 'off' );
end

if ( exist( handles.brcfil ) == 2 )
    set( [handles.PLOTRTH], 'enable', 'on' );
else
    set( [handles.PLOTRTH], 'enable', 'off' );
end

if ( exist( handles.shdfil ) == 2 | exist( handles.matfil ) == 2 ) % a matfil is assumed to contain TL data if exists
    set( [handles.PLOTSHD],   'enable', 'on' );
    set( [handles.PLOTTLR], 'enable', 'on' );
    set( [handles.PLOTTLD],   'enable', 'on' );
else
    set( [handles.PLOTSHD],   'enable', 'off' );
    set( [handles.PLOTTLR], 'enable', 'off' );
    set( [handles.PLOTTLD],   'enable', 'off' );
end

if ( exist( handles.prtfil ) == 2 )
    set( [handles.editPRTFIL], 'enable', 'on' );
else
    set( [handles.editPRTFIL], 'enable', 'off' );
end

% --- Executes on button press in editENVFILnew.
function editENVFIL_Callback(hObject, eventdata, handles)
% hObject    handle to editENVFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    eval( [ '!notepad ' handles.envfil ] );
end

% --- Executes on button press in editATIFILnew.
function editATIFIL_Callback(hObject, eventdata, handles)
% hObject    handle to editATIFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( [ '!notepad ' handles.atifil ] );

% --- Executes on button press in editBTYFILnew.
function editBTYFIL_Callback(hObject, eventdata, handles)
% hObject    handle to editBTYFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( [ '!notepad ' handles.btyfil ] );

% --- Executes on button press in editFIELDnew.
function editFIELD_Callback(hObject, eventdata, handles)
% hObject    handle to editFIELD (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( '!notepad field.flp' );

% --- Executes during object creation, after setting all properties.
function FigNum_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FigNum (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

function FigNum_Callback(hObject, eventdata, handles)
% hObject    handle to FigNum (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of FigNum as text
%        str2double(get(hObject,'String')) returns contents of FigNum as a double

function MakeFig( handles )

FigNum = str2double( get( handles.FigNum, 'String' ) );

if ( get( handles.Overlay, 'Value' ) )   % use existing figure if 'overlay' button is selected
    figure( FigNum )
else
    FigNum = FigNum + 1;
    % handles.FigNum = num2str( FigNum );
    set( handles.FigNum, 'String', num2str( FigNum ) );
    figure( FigNum )
end

% --- Executes on button press in editPRTFIL.
function editPRTFIL_Callback(hObject, eventdata, handles)
% hObject    handle to editPRTFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( [ '!notepad ' handles.prtfil ] );


% --- Executes on button press in BELLHOPHelp.
function BELLHOPHelp_Callback(hObject, eventdata, handles)
% hObject    handle to BELLHOPHelp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BELLHOPHelp
eval( [ '!notepad ' handles.atdoc 'bellhop.hlp' ] );

% --- Executes on button press in BounceHelp.
function BOUNCEHelp_Callback(hObject, eventdata, handles)
% hObject    handle to BounceHelp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BounceHelp
eval( [ '!notepad ' handles.atdoc 'bounce.hlp' ] );

% --- Executes on button press in KRAKENHelp.
function KRAKENHelp_Callback(hObject, eventdata, handles)
% hObject    handle to KRAKENHelp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of KRAKENHelp

eval( [ '!notepad ' handles.atdoc 'kraken.hlp' ] );

% --- Executes on button press in SCOOTERHelp.
function SCOOTERHelp_Callback(hObject, eventdata, handles)
% hObject    handle to SCOOTERHelp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of SCOOTERHelp

eval( [ '!notepad ' handles.atdoc 'scooter.hlp' ] );

% --- Executes on button press in SPARCHelp.
function SPARCHelp_Callback(hObject, eventdata, handles)
% hObject    handle to SPARCHelp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of SPARCHelp

eval( [ '!notepad ' handles.atdoc 'sparc.hlp' ] );

function rrt_Callback(hObject, eventdata, handles)
% hObject    handle to rrt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of rrt as text
%        str2double(get(hObject,'String')) returns contents of rrt as a double


% --- Executes during object creation, after setting all properties.
function rrt_CreateFcn(hObject, eventdata, handles)
% hObject    handle to rrt (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
