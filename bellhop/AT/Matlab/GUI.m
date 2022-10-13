function varargout = GUI(varargin)
% GUI M-file for GUI.fig
%      GUI, by itself, creates a new GUI or raises the existing
%      singleton*.
%
%      H = GUI returns the handle to a new GUI or the handle to
%      the existing singleton*.
%
%      GUI('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in GUI.M with the given input arguments.
%
%      GUI('Property','Value',...) creates a new GUI or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before GUI_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to GUI_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help GUI

% Last Modified by GUIDE v2.5 29-Nov-2002 15:21:49

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @GUI_OpeningFcn, ...
    'gui_OutputFcn',  @GUI_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);
if nargin & isstr(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before GUI is made visible.
function GUI_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to GUI (see VARARGIN)

% Choose default command line output for GUI
handles.output = hObject;
handles.envfil = '';
handles.atifil = '';
handles.btyfil = '';
handles.brcfil = '';
handles.trcfil = '';
handles.sbpfil = '';


% Update handles structure
guidata(hObject, handles);

% UIWAIT makes GUI wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = GUI_OutputFcn(hObject, eventdata, handles)
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

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    if ( exist( handles.atifil ) == 2 )
        copyfile( handles.atifil, 'atifil' );	% need altimetry  in atifil
    end
    if ( exist( handles.btyfil ) == 2 )
        copyfile( handles.btyfil, 'btyfil' );	% need bathymetry in btyfil
    end
    if ( exist( handles.trcfil ) == 2 )
        copyfile( handles.trcfil, 'trcfil' );	% need refl. coef in trcfil
    end
    if ( exist( handles.brcfil ) == 2 )
        copyfile( handles.brcfil, 'brcfil' );	% need refl. coef in brcfil
    end
    if ( exist( handles.sbpfil ) == 2 )
        copyfile( handles.sbpfil, 'sbpfil' );	% need refl. coef in brcfil
    end
    
    eval( [ '! bellhop.exe < ' handles.envfil ] );
end

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


% --- Executes on selection change in popupmenu1.
function popupmenu1_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popupmenu1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu1


% --- Executes during object creation, after setting all properties.
function popupmenu2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on selection change in popupmenu2.
function popupmenu2_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popupmenu2 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu2


% --------------------------------------------------------------------
function KRAKEN_Callback(hObject, eventdata, handles)
% hObject    handle to KRAKEN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    if ( exist( handles.btyfil ) == 2 )
        copyfile( handles.btyfil, 'btyfil' );	% need bathymetry in btyfil
    end
    if ( exist( handles.trcfil ) == 2 )
        copyfile( handles.trcfil, 'trcfil' );	% need refl. coef in trcfil
    end
    if ( exist( handles.brcfil ) == 2 )
        copyfile( handles.brcfil, 'brcfil' );	% need refl. coef in brcfil
    end
    eval( [ '! kraken.exe < ' handles.envfil ] );
    ! field.exe < field.flp
end


% --------------------------------------------------------------------
function SCOOTER_Callback(hObject, eventdata, handles)
% hObject    handle to SCOOTER (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    if ( exist( handles.btyfil ) == 2 )
        copyfile( handles.btyfil, 'btyfil' );	% need bathymetry in btyfil
    end
    if ( exist( handles.trcfil ) == 2 )
        copyfile( handles.trcfil, 'trcfil' );	% need refl. coef in trcfil
    end
    if ( exist( handles.brcfil ) == 2 )
        copyfile( handles.brcfil, 'brcfil' );	% need refl. coef in brcfil
    end
    eval( [ '! scooter.exe < ' handles.envfil ] );
    ! fields.exe < fields.flp
end


% --------------------------------------------------------------------
function KRAKENC_Callback(hObject, eventdata, handles)
% hObject    handle to KRAKENC (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    if ( exist( handles.btyfil ) == 2 )
        copyfile( handles.btyfil, 'btyfil' );	% need bathymetry in btyfil
    end
    if ( exist( handles.trcfil ) == 2 )
        copyfile( handles.trcfil, 'trcfil' );	% need refl. coef in trcfil
    end
    if ( exist( handles.brcfil ) == 2 )
        copyfile( handles.brcfil, 'brcfil' );	% need refl. coef in brcfil
    end
    eval( [ '! krakenc.exe < ' handles.envfil ] );
    ! field.exe < field.flp
end

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
    root = handles.envfil( 1:length( handles.envfil ) - 4 );
    handles.atifil = [ root '.ati' ];
    handles.btyfil = [ root '.bty' ];
    handles.trcfil = [ root '.trc' ];
    handles.brcfil = [ root '.brc' ];
    handles.sbpfil = [ root '.sbp' ];
end

% Update handles structure
guidata(hObject, handles);

% --------------------------------------------------------------------
function Edit_Callback(hObject, eventdata, handles)
% hObject    handle to Edit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function BOUNCE_Callback(hObject, eventdata, handles)
% hObject    handle to BOUNCE (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    eval( [ '! bounce.exe < ' handles.envfil ] );
end

% --------------------------------------------------------------------
function PLOTRAY_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTRAY (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( exist( 'RAYFIL' ) == 2 )
    plotray( 'RAYFIL' )
else
    warndlg( 'No ray file exists; you must run BELLHOP first (with ray ouput selected)', 'Warning' );
end

% --------------------------------------------------------------------
function PLOTSHD_Callback(hObject, eventdata, handles)
% hObject    handle to plotshd (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( exist( 'SHDFIL' ) == 2 )
    plotshd( 'SHDFIL' )
    handles
    caxisdat = eval( get( handles.caxis_vec, 'String' ) )
    %if exist( 'caxisdat' )
        caxis( caxisdat ); colorbar( 'horiz' )
    %end
else
    warndlg( 'No shdfil exists; you must run a model first', 'Warning' );
end

% --------------------------------------------------------------------
function PLOTBTY_Callback(hObject, eventdata, handles)
% hObject    handle to Bathymetry (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.btyfil ) )
    warndlg( 'No btyfil has been selected', 'Warning' );
else
    plotbty( handles.btyfil )
end

% --------------------------------------------------------------------
function PLOTATI_Callback(hObject, eventdata, handles)
% hObject    handle to Altimetry (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.atifil ) )
    warndlg( 'No atifil has been selected', 'Warning' );
else
    plotati( handles.atifil )
end


% --------------------------------------------------------------------
function PLOTRTH_Callback(hObject, eventdata, handles)
% hObject    handle to Refco (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


if ( exist( 'BRCFIL' ) == 2 )
    plotrth( 'BRCFIL' )
else
    warndlg( 'No bottom refl. coef. file exists; you must run BOUNCE first', 'Warning' );
end



% --------------------------------------------------------------------
function PLOTMODE_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTMODE (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( exist( 'MODFIL0001' ) == 2 )
    plotmode( 'MODFIL0001' )
else
    warndlg( 'No mode file exists; you must run KRAKEN first', 'Warning' );
end



% --------------------------------------------------------------------
function NewFigure_Callback(hObject, eventdata, handles)
% hObject    handle to NewFigure (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

figure


% ---------------------------------------------------------------------

% --- Executes just before lbox2 is made visible.
function lbox2_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to lbox2 (see VARARGIN)

% Choose default command line output for lbox2
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

if nargin == 3,
    initial_dir = pwd;
elseif nargin > 4
    if strcmpi(varargin{1},'dir')
        if exist(varargin{2},'dir')
            initial_dir = varargin{2};
        else
            errordlg('Input argument must be a valid directory','Input Argument Error!')
            return
        end
    else
        errordlg('Unrecognized input argument','Input Argument Error!');
        return;
    end
end
% Populate the listbox
load_listbox(initial_dir,handles)
% Return figure handle as first output argument

% UIWAIT makes lbox2 wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = lbox2_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

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


% --- Executes on selection change in listbox3.
function listbox3_Callback(hObject, eventdata, handles)
% hObject    handle to listbox3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox3


% --------------------------------------------------------------------
function EditENVFIL_Callback(hObject, eventdata, handles)
% hObject    handle to EditENVFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.envfil ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    eval( [ '!notepad ' handles.envfil ] );
end

% --------------------------------------------------------------------
function Untitled_1_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function EditATIFIL_Callback(hObject, eventdata, handles)
% hObject    handle to EditATIFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if ( isempty( handles.atifil ) )
    warndlg( 'No atifil has been selected', 'Warning' );
else
    eval( [ '!notepad ' handles.atifil ] );
end


% --------------------------------------------------------------------
function EditBTYFIL_Callback(hObject, eventdata, handles)
% hObject    handle to EditBTYFIL (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( isempty( handles.btyfil ) )
    warndlg( 'No btyfil has been selected', 'Warning' );
else
    eval( [ '!notepad ' handles.btyfil ] );
end



% --------------------------------------------------------------------
function EditFIELDS_Callback(hObject, eventdata, handles)
% hObject    handle to EditFIELDS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( '!notepad fields.flp' );

% --------------------------------------------------------------------
function EditFIELD_Callback(hObject, eventdata, handles)
% hObject    handle to EditFIELD (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

eval( '!notepad field.flp' );


% --------------------------------------------------------------------
function PLOTSLICE_Callback(hObject, eventdata, handles)
% hObject    handle to PLOTSLICE (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ( exist( 'SHDFIL' ) ~= 2 )
    warndlg( 'No shdfil exists; you must run a model first', 'Warning' );
else
    rdt = str2double( get( handles.rdt, 'String' ) )
    plotslic( 'SHDFIL', rdt )
    %end
end


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


