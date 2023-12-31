function varargout = HFdataRead(varargin)
if nargin == 0  % LAUNCH GUI
    token = 'HFDATAREAD_open';
    oldfig = getappdata(0, token);
    oldfig = oldfig(ishandle(oldfig));
    
    [fpath,n,e,v] = fileparts(which('kazan'));
    inifilename = [fpath '\kazan.ini'];
    ini = inimanage(inifilename);
    opc = 2;
    try opc = ini.KazanViewer.MultyKazan; end
    if (opc == 1| opc == 2) & ~isempty(oldfig), set(oldfig(1),'Visible','on'); varargout{1}=oldfig(1); return; end

	fig = openfig(mfilename,'new');
    setappdata(0, token, [oldfig, fig]);

	% Generate a structure of handles to pass to callbacks, and store it. 
	handles = guihandles(fig);
    hahdles.hh = 0;
    handles.mode = 3;
    guidata(fig, handles);

	if nargout > 0
		varargout{1} = fig;
	end

elseif ischar(varargin{1}) % INVOKE NAMED SUBFUNCTION OR CALLBACK
	try
		[varargout{1:nargout}] = feval(varargin{:}); % FEVAL switchyard
	catch
		disp(lasterr);
	end
end
% End initialization code - DO NOT EDIT


% --- Executes just before HFdataRead is made visible.
function HFdataRead_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to HFdataRead (see VARARGIN)

% Choose default command line output for HFdataRead
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes HFdataRead wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = HFdataRead_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

% --------------------------------------------------------------------
function varargout = FileUpdate(varargin)

function eRe1_Callback(hObject, eventdata, handles)


function eRe2_Callback(hObject, eventdata, handles)

function eIm1_Callback(hObject, eventdata, handles)

function eIm2_Callback(hObject, eventdata, handles)

function eFld_Callback(hObject, eventdata, handles)

% phase correction objects
% ------------------------
function ePhReIm_Callback(hObject, eventdata, handles)
Plott(handles);
function ePh_Callback(hObject, eventdata, handles)
Plott(handles);
function ePh1_Callback(hObject, eventdata, handles)
Plott(handles);

function slPhReIm_Callback(hObject, eventdata, handles)
num = str2num(get(handles.ePhReIm, 'String'));
val = num + (get(handles.slPhReIm, 'Value')-0.5)*2;
set(handles.ePhReIm, 'String', num2str(val));
Plott(handles);

function slPhRe_Callback(hObject, eventdata, handles)
num = str2num(get(handles.ePh, 'String'));
val = num + (get(handles.slPhRe, 'Value')-0.5)*2;
set(handles.ePh, 'String', num2str(val));
Plott(handles);

function slPhIm_Callback(hObject, eventdata, handles)
num = str2num(get(handles.ePh1, 'String'));
val = num + (get(handles.slPhIm, 'Value')-0.5)*2;
set(handles.ePh1, 'String', num2str(val));
Plott(handles);
% ------------------------

function pbCalculate_Callback(hObject, eventdata, handles)
Plott(handles)

function Plott(ha)
h = guidata(ha.figure1);
hand = guidata(h.hh);
hand.out = {};

str = get(ha.eRe1,'String');
nRe1 = str2num(str);
str = get(ha.eRe2,'String');
nRe2 = str2num(str);
str = get(ha.eIm1,'String');
nIm1 = str2num(str);
str = get(ha.eIm2,'String');
nIm2 = str2num(str);
str = get(ha.eFld,'String');
nFld = str2num(str);


str = get(ha.ePhReIm,'String');
PhReIm = str2num(str) * pi/180;
str = get(ha.ePh,'String');
Ph = str2num(str) * pi/180;
str = get(ha.ePh1,'String');
Ph1 = str2num(str) * pi/180;

Re1 = hand.src.y(:,nRe1);
Re2 = hand.src.y(:,nRe2);
Im1 = hand.src.y(:,nIm1);
Im2 = hand.src.y(:,nIm2);
if nFld
    Fld = hand.src.y(:,nFld);
else
    Fld = [0:(length(Re1)-1)]';
end

if get(ha.chkSplFld, 'Value')
    tc = str2num(get(ha.edTimeConst, 'String'));
    pol = str2num(get(ha.edPol, 'String'));    
    Fld = kv_mvgavg(Fld, tc, 'savgol', pol);
end
Re  = exp(-i*Ph)*(Re1 + i*Re2);
Im  = exp(-i*Ph1)*(Im1 + i*Im2);
ReIm  = exp(-i*PhReIm)*(Re + i*Im);

hand.out{1} = [];
hand.out{1}.ax = hand.src.ax;
hand.out{1}.ax.x = Fld;
hand.out{1}.ax.xlabel = 'Magnetic Field';
hand.out{1}.ax.y = [1];
hand.out{1}.ax.ylabel = '';

switch(ha.mode)
    case 1, yy = Re;
    case 2, yy = Im;
    case 3, yy = ReIm;
    case 4, yy = Fld; hand.out{1}.ax.x = [1:length(Fld)]'; hand.our{1}.ax.xlabel ='Points';
end
if get(ha.chkInt, 'Value'), % make integration
    hand.out{1}.y = cumsum(yy);
else
    hand.out{1}.y = yy;
end

guidata(h.hh, hand);
[path,name,ext,ver] = fileparts(get(h.hh, 'FileName'));
eval([name '(''SetDataSource'', 2, hand)']);

% --- Executes on button press in tbRe.
function tbRe_Callback(hObject, eventdata, handles)
group = [handles.tbRe, handles.tbIm, handles.tbFinal, handles.tbField];
set(group, 'Value', 0);
set(hObject, 'Value', 1)

handles.mode = find(group == hObject);
guidata(handles.figure1, handles);
Plott(handles)


function chkSplFld_Callback(hObject, eventdata, handles)
if get(handles.chkSplFld, 'Value')
    str = 'on';
else
    str = 'off';
end
set(handles.edTimeConst, 'Enable', str);
set(handles.edPol, 'Enable', str);
set([handles.text14, handles.text15], 'Enable', str);

Plott(handles);

function chkInt_Callback(hObject, eventdata, handles)
Plott(handles);


function edTimeConst_Callback(hObject, eventdata, handles)
Plott(handles);
function edPol_Callback(hObject, eventdata, handles)
Plott(handles);


