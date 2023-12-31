function varargout = fittingbox(varargin)
% FittingBox Application M-file for fittingbox.fig
%    Intended to use only with 'kazan' viewer

% KAZAN dataviewer with plugins By Boris Epel & Alexey Silakov
% MPI of Bioinorganic Chemistry, Muelhaim an der Ruhr, 2003
% Free for non-commercial use. Use the program at your own risk. 
% The authors retains all rights. 
% Contact: epel@mpi-muelheim.mpg.de

% Last Modified by GUIDE v2.5 02-May-2005 13:45:50
% Silakov Alexey
% AlSi 13.03.2004, 15:00

if nargin == 0  % LAUNCH GUI
    token = 'FITTINGBOX_open';
    oldfig = getappdata(0, token);
    oldfig = oldfig(ishandle(oldfig));
    
    [fpath] = fileparts(which('kazan'));
    inifilename = [fpath '\kazan.ini'];
    ini = inimanage(inifilename);
    opc = 2;
    try opc = ini.KazanViewer.MultyKazan; catch; end
    if (opc == 1 || opc == 2) && ~isempty(oldfig), set(oldfig(1),'Visible','on'); varargout{1}=oldfig(1); return; end

	fig = openfig(mfilename,'new');
    setappdata(0, token, [oldfig, fig]);
    
    % Generate a structure of handles to pass to callbacks, and store it. 
    handles = guihandles(fig);
    handles.hh = 0;
    handles.ploter = 0;
    handles.num = 1;
    handles.div = 0;
    handles.Data = [];
    handles.Script{1} = get(handles.list, 'String');
    handles.sim.y = [];
    handles.loadtype = 'onebyone'; % or 'allinone'
    handles.Color =   [1, 0, 0;...
            1, 1, 0;...
            0, 1, 0;...
            0, 0, 0;...
            1, 0, 1;...
            0, 1, 1;...
            1, 1, 0];
    handles.process = 1;
    handles.range.pnts = [];
    handles.range.bracket = [];
    handles.range.showbnds = 0;
    handles.showranges = 1;
    %   set(handles.butCol, 'BackgroundColor', handles.Color(1, :));
    guidata(fig, handles);
    figure1_ResizeFcn(handles.figure1, [], handles)
    list_Callback([], [], handles);
    
    if nargout > 0
        varargout{1} = fig;
    end
    
elseif ischar(varargin{1}) % INVOKE NAMED SUBFUNCTION OR CALLBACK
    
    try
        %     [varargout{1:nargout}] = 
        feval(varargin{:}); % FEVAL switchyard
    catch
        disp(lasterr);
    end
    
end


%| ABOUT CALLBACKS:
%| GUIDE automatically appends subfunction prototypes to this file, and 
%| sets objects' callback properties to call them through the FEVAL 
%| switchyard above. This comment describes that mechanism.
%|
%| Each callback subfunction declaration has the following form:
%| <SUBFUNCTION_NAME>(H, EVENTDATA, HANDLES, VARARGIN)
%|
%| The subfunction name is composed using the object's Tag and the 
%| callback type separated by '_', e.g. 'slider2_Callback',
%| 'figure1_CloseRequestFcn', 'axis1_ButtondownFcn'.
%|
%| H is the callback object's handle (obtained using GCBO).
%|
%| EVENTDATA is empty, but reserved for future use.
%|
%| HANDLES is a structure containing handles of components in GUI using
%| tags as fieldnames, e.g. handles.figure1, handles.slider2. This
%| structure is created at GUI startup using GUIHANDLES and stored in
%| the figure's application data using GUIDATA. A copy of the structure
%| is passed to each callback.  You can store additional information in
%| this structure at GUI startup, and you can change the structure
%| during callbacks.  Call guidata(h, handles) after changing your
%| copy to replace the stored original so that subsequent callbacks see
%| the updates. Type "help guihandles" and "help guidata" for more
%| information.
%|
%| VARARGIN contains any extra arguments you have passed to the
%| callback. Specify the extra arguments by editing the callback
%| property in the inspector. By default, GUIDE sets the property to:
%| <MFILENAME>('<SUBFUNCTION_NAME>', gcbo, [], guidata(gcbo))
%| Add any extra arguments after the last argument, before the final
%| closing parenthesis.
% --------------------------------------------------------------------
function varargout = figure1_DeleteFcn(h, eventdata, handles, varargin)
[path,name] = fileparts(get(handles.hh, 'FileName'));
eval([name '(''DeletePlugins'', handles.figure1, handles.hh)']);

% --------------------------------------------------------------------
function varargout = figure1_CloseRequestFcn(h, eventdata, handles, varargin)
delete(handles.figure1);

% --------------------------------------------------------------------
function FileUpdate(varargin);
return;
% --------------------------------------------------------------------
function varargout = list_Callback(h, eventdata, handles, varargin)
% % Stub for Callback of the uicontrol handles.listbox1.
Res = get(handles.list, 'String');
val = get(handles.list, 'Value');

[name, str1] = strtok(Res{val}, '=');
str2 = [strtrim(str1(2:end)), ' '];

if strcmp(strtrim(name), 'f')
    set(handles.ePars, 'String', str2);
    set(handles.ePars, 'UserData', 'function');
elseif str2(1)==''''
    primes = findstr(str2, '''');
    if size(primes, 2) < 2, primes(2) = size(str2, 2); end
    set(handles.ePars, 'String', str2(2:(primes(2)-1)));
    set(handles.ePars, 'UserData', 'string');
else
    dig = str2num(str2);
    set(handles.ePars, 'String', num2str(dig, 6));
    set(handles.ePars, 'UserData', 'value');
end

% --------------------------------------------------------------------
function varargout = butLoadData_Callback(h, eventdata, handles, varargin)
hand = guidata(handles.hh);
if isempty(hand),  
    msgbox('Can not find main box handles', 'Error');
    return;
end
[path,name] = fileparts(hand.fname);
file_str = name;
sizey = size(hand.src.y, 2);
if sizey==1
    button = 'All in One';
else
    button = questdlg('How to load?',...
    '???','One by One','All in One','Cancel','One by One');        
end
popString = {};
switch button
    case 'One by One'
        if sizey > 10, 
            button = questdlg('Do you realy want to load data one by one?',...
                ['there are ' num2str(sizey), ' columns'],...
                'Oh, ja ja','Nie','Nie');
            if strcmp(button, 'Nie'), return; end
        end
        handles.loadtype = 'onebyone';
        for i = 1:sizey
            handles.Script{i} = get(handles.list, 'String');
            popString{end+1} = [file_str, '_col_ ', num2str(i)];
        end
    case 'All in One'
        handles.loadtype = 'allinone';
        handles.Script{1} = get(handles.list, 'String');
        popString = {file_str};
    otherwise
        return;
end
handles.sim.y = hand.src.y*0;
handles.src = hand.src;
% if ~isfield(handles, 'range'),
%     switch hand.Prj
%         case 'X'
%             handles.range.pnts = [1:size(hand.src.y, 1)];
%             handles.range.barcket = [1, size(hand.src.y, 1)];
%         case 'Y'
%             handles.range.pnts = [1:size(hand.src.y, 2)];
%             handles.range.barcket = [1, size(hand.src.y, 2)];
%     end
% end

set(handles.popSel, 'String', popString, 'Value', 1);
set(handles.list, 'String', handles.Script{1}, 'Value', 1);
guidata(handles.figure1, handles);

% --------------------------------------------------------------------
function varargout = ePars_Callback(h, eventdata, handles, varargin)
% % Stub for Callback of the uicontrol handles.ePars.
Res = get(handles.list, 'String');
val = get(handles.list, 'Value');
newval = strtrim(get(handles.ePars, 'String'));

[name, str1] = strtok(Res{val}, '=');
name = strtrim(name);

switch get(handles.ePars, 'UserData')
    case 'function'
        set(handles.ePars, 'String', newval);
        
        tokens = kv_autofit([name ' = ' newval]);
        
        %%%%%%%%%%%%% load script %%%%%%%%%%%%%%%%%%%%%%
        for k=1:size(Res,1),
            if ~strcmp(strtrim(strtok(Res{k},'=')),'f')
                try eval([Res{k} ';']);end
            end
        end
        tempcell = Res;
        Res = {[name ' = ' newval]};
        for k = 1: size(tokens, 2)
            try, var = eval(tokens{k}); catch, var = 0; end
            Res{end+1} = [tokens{k},' = ', num2str(var)];
        end

        clear tempcell;
    case 'string'
        set(handles.ePars, 'string', newval);
        Res{val} = [name ' = ''', newval,''''];
    otherwise
        nums = str2num(newval);
        set(handles.ePars, 'String', num2str(nums));
        Res{val} = [name ' = ' num2str(nums)];
end
set(handles.list, 'String', Res);
list_Callback(h, eventdata, handles);
switch handles.loadtype
    case 'onebyone', 
        handles.Script{get(handles.popSel, 'Value')} = Res;
    case 'allinone'
        handles.Script{1} = Res;
end
guidata(handles.figure1, handles);
hand = guidata(handles.hh);

if strcmp(get(handles.ePars, 'UserData'), 'value'),
    evallist(handles);
end

%--------------------------------------------------------------------
function varargout = slPars_Callback(h, eventdata, handles, varargin)
parname = get(handles.list, 'String');
parval = get(handles.list, 'Value');
if ~strcmp(get(handles.ePars, 'UserData'),'value'), return; end;

str = get(handles.popup, 'String');

shift = get(handles.slPars, 'Value');
set(handles.slPars, 'Value',0);
num = get(handles.popup, 'Value');

dim = str2num([str{num}]);

CurVal = str2double(get(handles.ePars, 'String'));
Val = CurVal + dim*shift;

set(handles.ePars, 'String', num2str(Val));
ePars_Callback(h, eventdata, handles, varargin);

%--------------------------------------------------------------------
function plothis(handles)
handles = guidata(handles.figure1);
colors = handles.Color;

hand = guidata(handles.hh);
ax = handles.src.ax;
y = handles.src.y;

[prj] = getslider_kazan(hand, size(y));
hand.out = {};
% ax = hand.src.ax;
% y = hand.src.y;

if handles.process, [y, ax] = processing(y, ax);
else
    ax.diff = '';
    ax.filt = '';
end
% Data
hand.out{1}.ax = ax;
hand.out{1}.ax.Color = [1 1 1]*0.4; % always gray ?
hand.out{1}.ax.s = 0;
hand.out{1}.y = y;
hand.out{1}.title = 'Src';

if handles.range.bracket,
    FitRng = handles.range.bracket;
    
    Dy = (max(max(real(y))) - min(min(real(y))))/50; 
    eval(['axxx = ax.', lower(prj), ';']);
    for count = 1:size(FitRng, 1)  
        hand.out{end+1}.ax = ax;
        
        if FitRng(count, 1) >length(axxx), FitRng(count, 1) = length(axxx); end
        if FitRng(count, 2) >length(axxx), FitRng(count, 2) = length(axxx); end
        %%%%%%%%%%%%%% Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        eval(['hand.out{end}.ax.', lower(prj),' = ax.', lower(prj),...
                '([FitRng(count, 1):FitRng(count, 2)], 1);']);
        hand.out{end}.ax.Color = [0, 0, 1]; % always blue ?
        hand.out{end}.ax.s = 0;
        hand.out{end}.title = ['Src', num2str(count)];
        
        switch prj
            case 'X'
                hand.out{end}.y = y([FitRng(count, 1):FitRng(count, 2)], :);                
            case 'Y'
                hand.out{end}.y = y(:, [FitRng(count, 1):FitRng(count, 2)]);
        end
        if handles.range.showbnds,
            %%%%%%%%%%%%%% Bra... %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            hand.out{end+1}.ax = ax;
            eval(['hand.out{end}.ax.', lower(prj),' = ax.', lower(prj),...
                    '(FitRng(count, [1, 1]), 1);']);
            hand.out{end}.ax.Color = 'b';
            hand.out{end}.ax.s = 0;
            hand.out{end}.title = ['br_', num2str(count)];
            
            switch prj
                case 'X'
                    hand.out{end}.y(1, :) = y(FitRng(count, 1), :) - Dy;
                    hand.out{end}.y(2, :) = y(FitRng(count, 1), :) + Dy;
                case 'Y'
                    hand.out{end}.y(:, 1) = y(:, FitRng(count, 1)) - Dy;
                    hand.out{end}.y(:, 2) = y(:, FitRng(count, 1)) + Dy;
            end        
            %%%%%%%%%%%%%% cket... %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            hand.out{end+1}.ax = ax;
            eval(['hand.out{end}.ax.', lower(prj),' = ax.', lower(prj),...
                    '(FitRng(count, [2, 2]), 1);']);
            hand.out{end}.ax.Color = 'b';
            hand.out{end}.ax.s = 0;
            hand.out{end}.title = ['er_', num2str(count)];
            
            switch prj
                case 'X'
                    hand.out{end}.y(1, :) = y(FitRng(count, 2), :) - Dy;
                    hand.out{end}.y(2, :) = y(FitRng(count, 2), :) + Dy;
                case 'Y'
                    hand.out{end}.y(:, 1) = y(:, FitRng(count, 2)) - Dy;
                    hand.out{end}.y(:, 2) = y(:, FitRng(count, 2)) + Dy;
            end   
        end
    end
else
    hand.out{1}.ax.Color = 'b'; 
end

% Result
hand.out{end + 1}.ax = ax;
hand.out{end}.y = handles.sim.y;
hand.out{end}.ax.Color = 'r';
hand.out{end}.ax.s = 0;
hand.out{end}.title = 'Out';

states = {'off', 'on'};
st = get(handles.mSimDif, 'Checked');
check = find(strcmp(states, st)) -1;
% Data - result
if check, 
    hand.out{end+1}.ax = ax;
    hand.out{end}.y = y - handles.sim.y;
    hand.out{end}.ax.Color = 'g';
    hand.out{end}.ax.s = 0;
    hand.out{end}.title = 'Diff';
end

guidata(handles.hh, hand);
[fpath,name] = fileparts(get(handles.hh, 'FileName'));
eval([name '(''SetDataSource'', 2, hand)']);

% ------------------------------------------------------------
function butCol_Callback(hObject, eventdata, handles)
num = get(handles.popSel, 'Value');
c = uisetcolor(handles.Color(num, :));
if size(c, 2) > 1
  
  handles.Color(num, :) = c;
  set(handles.butCol, 'BackgroundColor', c, 'ForegroundColor', [1 1 1] - c);
  guidata(handles.figure1, handles);
  plothis(handles);
end


% % ------------------------------------------------------------
function popSel_Callback(hObject, eventdata, handles)
num = get(handles.popSel, 'Value');
if strcmp(handles.loadtype, 'onebyone'),
    list = handles.Script{num};
    set(handles.list, 'String', list, 'Value', 1);
    
    hand = guidata(handles.hh);
    num = get(handles.popSel, 'Value');
    
    set(hand.sl2Dplot, 'Value', num);
    kazan('sl2Dplot_Callback',hand.sl2Dplot,[],hand);
else
    set(handles.list, 'String', handles.Script{num}, 'Value', 1);
end
list_Callback(handles.list, eventdata, handles);

% set(handles.butCol, 'BackgroundColor', handles.Color(num, :)); 

% --------------------------------------------------------------------
function varargout = nearme(X, xi)
% nearme(X, xi)
% k = nearme(X, xi)
% [k, pbshowhide] = nearme(X, xi)
% 
%   return value (pbshowhide) and index (k) 
%   of nearest to xi number of array X 

[c, k] = min(abs(X - ones(size(X))*xi));
v = X(k);
switch nargout 
    case 0
        varargout{1} = k;
    case 1
        varargout{1} = k;
    case 2
        varargout{1} = k;
        varargout{2} = v; 
end
%--------------------------------------------------------------------

function butFitt_Callback(h, eventdata, handles)
hand = guidata(handles.hh);
sliderval = get(hand.sl2Dplot, 'Value');

[prj, selec, trans] = getslider_kazan(hand, size(handles.src.y));

if ~isfield(handles,'src'), error('Please load your data!'); end

eval(['x  = handles.src.ax.', lower(prj), ';']);
eval(['y = handles.src.y', selec, trans, ';']);
[ss, ind] = max(size(y));
if ind == 2, x = x.'; end
num = get(handles.popSel, 'Value');

[strings,yres]= kv_autofit(handles.Script{num}, x, real(y), handles.range.pnts);
if strcmp(selec, '(:, :)'), sel = ''; else sel = selec; end
eval(['handles.sim.y', sel,'= yres',trans,';']);
try
    set(handles.list, 'String', strings);
catch
    set(handles.list, 'String', strings, 'Value', 1);
end
list_Callback(h, [], handles);

handles.Script{num} = strings;

guidata(handles.figure1, handles);
plothis(handles);

%--------------------------------------------------------------------
function butFittAll_Callback(h, eventdata, handles)
hand = guidata(handles.hh);
% sliderval = get(hand.sl2Dplot, 'Value');
[prj, selec] = getslider_kazan(hand);

switch hand.Projection
case 2
    x = handles.src.ax.y;
    y = real(handles.src.y)';
case 1
    x = handles.src.ax.x;
    y = real(handles.src.y);
end

numy = size(handles.src.y, ~(hand.Projection-1)+1);
switch handles.loadtype
case 'onebyone'
    for ii = 1:numy
        set(handles.popSel, 'Value', ii);
        strings = handles.Script{ii};
        set(handles.list, 'String', handles.Script{ii});
        y = handles.src.y(:, ii);
        [ss, ind] = max(size(y));
        if ind == 2, x = x.'; end
        
        [newstrings, yres] = kv_autofit(strings, x, y, handles.range.pnts);
        handles.sim.y(:, ii) = yres;
%         set(handles.list, 'String', script);
        handles.Script{ii} = newstrings;
    end
case 'allinone'
    %%%%%%%% construct fitting function %%%%%%%
    strings = get(handles.list, 'String');
    
    [newstrings, handles.sim.y] = kv_autofit(strings, x, y, handles.range.pnts);
    
    if hand.Projection==2, handles.sim.y = handles.sim.y'; end
    
    set(handles.list, 'String', newstrings);
    handles.Script{1} = strings;
end

guidata(handles.figure1, handles);
plothis(handles);

set(handles.popSel, 'Value', 1);
popSel_Callback(handles.popSel, [], handles);
    %--------------------------------------------------------------------
function evallist(handles)
str = get(handles.list, 'String');
str_range = get(handles.listbox2, 'String');
val = 0;
hand = guidata(handles.hh);
FitRng = [];
% AlSi 02.12.04
[prj, selec] = getslider_kazan(hand);
eval(['x = handles.src.ax.', lower(prj), ';']);
eval(['y = handles.src.y', selec, ';']);

[ss, ind] = max(size(y));
if ind == 2, x = x.'; end
for count = 1:size(str, 1)
    if ~strcmp(str{count}(1:3), 'f =')
        eval([str{count}, ';']);
    else
        val = count;
    end
end
if ~ischar(str_range)
    for count = 1:size(str_range, 1)
        eval([str_range{count}, ';']);
    end
end
% pnts = x*0;
handles.range.pnts = getrange(FitRng, length(x));
handles.range.bracket = FitRng;
% if ~isempty(FitRng)
%     for i = 1:size(FitRng, 1)
%         pnts([FitRng(i, 1):min(FitRng(i, 2), end)]) = 1;
%     end
%     handles.range.pnts = find(pnts);
%     handles.range.bracket = FitRng;
% end

if ~val, disp('EVALLIST: Can not find function'); return; end
if findstr(str{val}, 'polyfit') 
    butFitt_Callback(handles.butFitt, [], handles)
else
    eval([str{val}, ';']);
    if hand.PlotType~=1
        switch hand.Projection
            case 1
                handles.sim.y = f(:, ones(size(handles.src.y, 2), 1));
            case 2
                handles.sim.y = f(ones(size(handles.src.y, 1), 1), :);
        end
    else
        eval(['handles.sim.y', selec, ' = f;']);
    end
    guidata(handles.figure1, handles);
    plothis(handles);
end

%--------------------------------------------------------------------
function mSimDif_Callback(h, eventdata, handles)
states = {'off', 'on'};
st = get(handles.mSimDif, 'Checked');
check = find(strcmp(states, st))-1;

set(handles.mSimDif, 'Checked', states{~check + 1});
guidata(handles.figure1, handles);
plothis(handles);

%--------------------------------------------------------------------
function mSimFFunc_Callback(h, eventdata, handles)
Res = get(handles.list, 'String');
val = get(handles.list, 'Value');


fittingbox_func(handles);
uiwait(handles.figure1);
str = get(handles.list, 'UserData');
if isempty(str), return; end
%%%% find strings with additional parameters %%%
for count = 1:length(Res)
    if length(Res{count})>6,
        if strcmp(Res{count}(1:6), 'FitRng')
            str{end+1} = Res{count};
        end
    end
end

set(handles.list, 'String', str, 'Value', 1);
list_Callback(handles.list, [], handles);

% --------------------------------------------------------------------
function mSimAgr_Callback(h, eventdata, handles)
str = {'off', 'on'};
stat = strcmp(get(handles.mSimAgr, 'Checked'), 'on');
set(handles.mSimAgr, 'Checked', str{~stat + 1});
handles.process = ~stat;
guidata(handles.figure1, handles);
plothis(handles);

% --------------------------------------------------------------------
function mSimAddRng_Callback(h, eventdata, handles)
hand = guidata(handles.hh);
[path,name] = fileparts(get(handles.hh, 'FileName'));
axx =hand.out{1}.ax.x;
axy =hand.out{1}.y;

[x,y]=eval([name '(''GetPoint'', hand, 1)']);
tx(1) = nearme(axx, x); % in points
Ylim = get(gca, 'YLim');
linehand = line([1, 1]*axx(max(tx(1), 1)), Ylim, 'LineStyle', ':', 'Parent', gca); 

[x,y]=eval([name '(''GetPoint'', hand, 1)']);
tx(2) = nearme(axx, x); % in points
delete(linehand);
str = get(handles.listbox2, 'String');
chkrng = 0;
if ischar(str)
    chkrng = 0;    
elseif length(str{1})<6
    chkrng = 0;    
elseif ~strcmp(str{1}(1:6), 'FitRng')
    chkrng = 0;    
else 
    chkrng = 1;    
end
hand = guidata(handles.hh);
[prj, sel] = getslider_kazan(hand);
tx = sort(tx);

if isfield(handles, 'src')
    eval(['numends = (length(handles.src.ax.',lower(prj),'));']);
else
    numends  = 1;
end
if ~chkrng 
    str{end + 1} = ['FitRng(1, 1) = ', num2str(max(1, tx(1)))];
    str{end + 1} = ['FitRng(1, 2) = ', num2str(min(numends, tx(2)))];
    set(handles.pbKillRange, 'Enable', 'on');
else
    str{end + 1} = ['FitRng(end+1, 1) = ' num2str(max(1, tx(1)))];
    str{end + 1} = ['FitRng(end, 2) = ', num2str(min(numends, tx(2)))];
end
guidata(handles.figure1, handles);
set(handles.listbox2, 'String', str, 'Value', length(str));
evallist(handles);

% --------------------------------------------------------------------
function mSimShowBds_Callback(h, eventdata, handles)
handles = guidata(handles.figure1);
if ~handles.range.bracket, disp('no range settings'); return; end
str = {'off', 'on'};
stat = get(handles.mSimShowBds, 'Checked');
num = find(strcmp(str, stat))-1;
set(handles.mSimShowBds, 'Checked', str{~num+1});
handles.range.showbnds = ~num;
guidata(handles.figure1, handles);
plothis(handles);

% --------------------------------------------------------------------
function mSimDelRng_Callback(h, eventdata, handles)
if isempty(handles.range.bracket), disp('nothing to delete'); 
    set(handles.pbKillRange, 'Enable', 'off');
    return; 
end

list  = get(handles.listbox2, 'String');
num  = get(handles.listbox2, 'Value');
count = [];
FitRng = [];
% find strings with 'FitRng'
if ~ischar(list)
    for i = 1:length(list)
        eval([list{i}, ';']);
    end
end
if isempty(FitRng), 
    disp('nothing to delete'); 
    set(handles.pbKillRange, 'Enable', 'off');
    return; 
end

if length(list) ==2
    tlist = [];
    set(handles.pbKillRange, 'Enable', 'off');
elseif mod(length(list), 2) % just in case
    error('Odd number of stings ????');
else
    nn = (mod(num, 2)-0.5)*2; % if odd ->-1, else +1
    non = sort([num, num+nn]);
    I = [1:(non(1)-1), (non(2)+1):length(list)];
    tlist = list(I);

end
newsize = length(tlist);
%if (length(count) == 2)|(pos<0),
set(handles.listbox2, 'String', tlist, 'Value', min(num, newsize));
listbox2_Callback(h, [], guidata(handles.figure1));
handles.Script_Range = tlist;
guidata(handles.figure1, handles);
evallist(handles);
% --------------------------------------------------------------------
function points = getrange(FitRng, numpnts)
pnts = [1:numpnts]*0;
if FitRng
    for i = 1:size(FitRng, 1)
        pnts([min(FitRng(i, 1), end):min(FitRng(i, 2), end)]) = 1;
    end
    points = find(pnts);
else
    points = [];    
end



% --------------------------------------------------------------------
function varargout = popup_Callback(h, eventdata, handles, varargin)
% --------------------------------------------------------------------
function varargout = but1Step_Callback(h, eventdata, handles, varargin)
% --------------------------------------------------------------------

function [prj, sel, trans] = getslider_kazan(hand, max_xy)
str = {'X', 'Y'};
trans = '';
prj = str{hand.Projection};
if hand.PlotType ==1
    switch hand.Projection
        case 1
            sel = ['(:, ','min(',  num2str(hand.Selection),', ', num2str(max_xy(2)),'))'];
        case 2
            sel = ['(min(', num2str(hand.Selection),', end), :)'];
            trans = '''';
    end
else
    sel = '(:, :)';
end

% --------------------------------------------------------------------
function varargout = figure1_ResizeFcn(h, eventdata, handles, varargin)
brd = 5;
YPos = brd;
butw = 17;
sl = [1.2, 1]*butw;
figpos = get(handles.figure1, 'Position'); % points
inn = 0;
if figpos(3)<=100, figpos(3) = 100; inn = 1; end
if figpos(4)<=300, figpos(4) = 300; inn = 1; end
if inn,  set(handles.figure1, 'Position', figpos); end
% buttons
buth = butw;
bFAp = [brd, brd,figpos(3)-2*brd, buth];
bFp = bFAp;
bFp(2) = bFAp(2)+bFAp(4)+brd;
b1Sp =bFAp; 
b1Sp(2) = bFp(2)+bFp(4)+brd;
set(handles.butFittAll, 'position', bFAp);
set(handles.butFitt, 'position', bFp);
set(handles.but1Step, 'position', b1Sp);

Ypos = b1Sp(2)+b1Sp(4)+brd;
% fitting range
if safeget(handles, 'showranges', 1)
    parth = 0.2; % 30% of the Figure height
    
    fRp = [brd, Ypos, figpos(3)-2*brd, figpos(4)*parth];
    slRSp = [fRp(1)+brd, fRp(2)+brd, sl];
    edRVp = [slRSp(1)+slRSp(3) + brd, fRp(2)+brd, fRp(3) - slRSp(3)-2*butw-4*brd, butw];
    pbARp = [edRVp(1) + edRVp(3)+brd, fRp(2)+brd, butw, butw];
    pbKRp = [pbARp(1) + butw, fRp(2)+brd, butw, butw];
    lb2p =  [fRp(1)+brd, fRp(2)+butw + 2*brd, fRp(3)-2*brd, fRp(4)-4*brd - butw];
    txtRp = [fRp(1)+brd, fRp(2)+fRp(4)-8, 56, 12];
    pbSHp =  [fRp(1)+fRp(3)-30, fRp(2)+fRp(4)-7.5, 30, 7.5];
    
    set(handles.frRange, 'position', fRp);
    set(handles.slRangeStep, 'position', slRSp, 'Visible', 'on');
    set(handles.edRangeVal, 'position', edRVp, 'Visible', 'on');
    set(handles.pbAddRange, 'position', pbARp, 'Visible', 'on');
    set(handles.pbKillRange, 'position', pbKRp, 'Visible', 'on');
    set(handles.listbox2, 'position', lb2p, 'Visible', 'on');
    set(handles.txtRange, 'position', txtRp);
    set(handles.pbShowHide, 'position', pbSHp);
else
    fRp = [brd, Ypos, figpos(3)-2*brd, 12];
    txtRp = [fRp(1)+brd, fRp(2)+fRp(4)-8, 56, 10];
    pbSHp =  [fRp(1)+fRp(3)-30, fRp(2)+fRp(4)-7.5, 30, 7.5];
    set(handles.frRange, 'position', fRp);
    set(handles.txtRange, 'position', txtRp);
    set(handles.pbShowHide, 'position', pbSHp);
    set(handles.slRangeStep,'Visible', 'off');
    set(handles.edRangeVal, 'Visible', 'off');
    set(handles.pbAddRange,'Visible', 'off');
    set(handles.pbKillRange, 'Visible', 'off');
    set(handles.listbox2, 'Visible', 'off');
end
% Fitting settings
Ypos = fRp(2)+fRp(4)+brd;
frMp = [brd, Ypos, figpos(3)-2*brd, figpos(4)-Ypos-butw-4*brd];
ePp = [frMp(1)+brd, frMp(2)+brd, frMp(3)-2*brd, butw];
slPp = [frMp(1)+brd, frMp(2)+butw+2*brd, sl];
ppp = [frMp(1)+ 2*brd+sl(1), frMp(2)+butw+2*brd, frMp(3)-3*brd-sl(1), butw];
lstp = [frMp(1)+brd, frMp(2)+3*brd+2*butw, frMp(3)-2*brd, frMp(4)-4*brd-2*butw];
txtMp = [frMp(1)+brd, frMp(2)+frMp(4)-5, 56, 10];

set(handles.frMain, 'position', frMp);
set(handles.ePars, 'position', ePp);
set(handles.slPars, 'position', slPp);
set(handles.popup, 'position', ppp);
set(handles.list, 'position', lstp);
set(handles.txtMain, 'position', txtMp);

Ypos = frMp(2)+frMp(4)+brd;
% Data selector
frChp = [brd, Ypos, figpos(3)-2*brd, butw+2*brd];
popSp = [frChp(1)+brd, frChp(2)+brd, frChp(3)-3*brd - 25, butw];
butCp = [frChp(1)+2*brd + popSp(3), frChp(2)+brd, 25, butw];
txtDp = [frChp(1)+brd, frChp(2)+frChp(4)-5, 56, 10];
set(handles.frChoose, 'position', frChp);
set(handles.popSel, 'position', popSp);
set(handles.butCol, 'position', butCp);
set(handles.txtDatas, 'position', txtDp);

% -----------------------------------------------------------------
function pbShoHide_Callback(h, eventdata, handles, varargin)
handles.showranges = ~handles.showranges;
if strcmp(get(handles.pbShowHide, 'String'), 'v')
    set(handles.pbShowHide, 'String', 'A');
else
    set(handles.pbShowHide, 'String', 'v');
end
guidata(handles.figure1, handles);
figure1_ResizeFcn(handles.figure1, [], handles)

% -----------------------------------------------------------------
function listbox2_Callback(h, eventdata, handles, varargin)
Res = get(handles.listbox2, 'String');
val = get(handles.listbox2, 'Value');
if val==0, set(handles.edRangeVal, 'String', 0); return; end
[name, str1] = strtok(Res{val}, '=');
str2 = [strtrim(str1(2:end)), ' '];

dig = str2num(str2);
set(handles.edRangeVal, 'String', num2str(dig, 6));
set(handles.edRangeVal, 'UserData', 'value');

% -----------------------------------------------------------------
function edRangeVal_Callback(h, eventdata, handles, varargin)
% % Stub for Callback of the uicontrol handles.ePars.
Res = get(handles.listbox2, 'String');
val = get(handles.listbox2, 'Value');
newval = strtrim(get(handles.edRangeVal, 'String'));

[name, str1] = strtok(Res{val}, '=');
name = strtrim(name);

nums = str2num(newval);
set(handles.edRangeVal, 'String', num2str(nums));
Res{val} = [name ' = ' num2str(nums)];

set(handles.listbox2, 'String', Res);
listbox2_Callback(h, eventdata, handles);

handles.Script_Range = Res;

guidata(handles.figure1, handles);
hand = guidata(handles.hh);

evallist(handles);

%--------------------------------------------------------------------
function varargout = slRangeStep_Callback(h, eventdata, handles, varargin)
parname = get(handles.listbox2, 'String');
parval = get(handles.listbox2, 'Value');
if ~strcmp(get(handles.edRangeVal, 'UserData'),'value'), return; end

shift = get(handles.slRangeStep, 'Value');
set(handles.slRangeStep, 'Value',0);
dim = 1;

CurVal = str2double(get(handles.edRangeVal, 'String'));
Val = (CurVal + shift);

set(handles.edRangeVal, 'String', num2str(Val));
edRangeVal_Callback(h, eventdata, handles, varargin);