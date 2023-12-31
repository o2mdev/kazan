function varargout = image2data(varargin)
% IMAGE2DATA Application M-file for image2data.fig
%    Intended to use only with 'kazan' viewer

% KAZAN dataviewer with plugins By Boris Epel & Alexey Silakov
% MPI of Bioinorganic Chemistry, Muelhaim an der Ruhr, 2003
% Free for non-commercial use. Use the program at your own risk. 
% The authors retains all rights. 
% Contact: epel@mpi-muelheim.mpg.de

% Last Modified by GUIDE v2.5 05-Oct-2004 22:54:47
% Alexey Silakov & Boris Epel 5-dec-2003, MPI
% boep 10-dec-2003, MPI

if nargin == 0  % LAUNCH GUI
    token = 'IMAGE2DATA_open';
    oldfig = getappdata(0, token);
    oldfig = oldfig(ishandle(oldfig));
    
    [fpath] = fileparts(which('kazan'));
    inifilename = [fpath '\kazan.ini'];
    ini = inimanage(inifilename);
    opc = 2;
    try opc = ini.KazanViewer.MultyKazan; catch, end
    if (opc == 1 || opc == 2) & ~isempty(oldfig), set(oldfig(1),'Visible','on'); varargout{1}=oldfig(1); return; end

	fig = openfig(mfilename,'new');
    setappdata(0, token, [oldfig, fig]);
    
    % Generate a structure of handles to pass to callbacks, and store it. 
    handles = guihandles(fig);    
    % seting default script
    set(fig, 'Units', 'pixels');
    allchld = get(fig, 'Children');
    getchld = findobj(fig, 'type', 'uicontrol');
    set(getchld, 'Units', 'pixels');
    defscr = {'Selected_color = ''grey''';...
              'Lower_color_level = 0.1';...
              'Background_color = [1, 1, 1]';...              
              'Region_left = 0';... 
              'Region_right = 0';...
              'Region_top = 0';...
              'Region_bottom = 0';...              
              'First_Xpoint = 0';...
              'First_Xvalue = 0';...
              'Last_Xpoint = 10';...
              'Last_Xvalue = 10';...
              'First_Ypoint = 0';...
              'First_Yvalue = 0';...
              'Last_Ypoint = 10';...
              'Last_Yvalue = 10';...
              'nPoints = 500'
              'Units = ''?'''...
            };
    set(handles.list, 'String', defscr);
    handles.operate = 0;
    handles.mSelLeftBottom = uimenu(fig, 'Label','Dim LB', 'Callback', 'image2data(''SelLeftBottom_Callback'', gcbo, [], guidata(gcbo))', ...
                    'Tag', 'mSelLeftBottom');                    
    handles.mSelRightTop = uimenu(fig, 'Label','Dim RT', 'Callback', 'image2data(''SelRightTop_Callback'', gcbo, [], guidata(gcbo))', ...
                    'Tag', 'mSelRightTop');                    
    handles.mSelRegion = uimenu(fig, 'Label','SelectRegion', 'Callback', 'image2data(''SelReg_Callback'', gcbo, [], guidata(gcbo))', ...
                    'Tag', 'mSelRegion');                    
    handles.mEraser = uimenu(fig, 'Label','+ Eraser', 'Callback', 'image2data(''Eraser_Callback'', gcbo, [], guidata(gcbo))', ...
                    'Tag', 'mEraser');
    handles.mKillEraser = uimenu(fig, 'Label','- Eraser', 'Callback', 'image2data(''KillEraser_Callback'', gcbo, [], guidata(gcbo))', ...
                    'Tag', 'mKillEraser');    
    handles.exclude = [];
    Figure_ResizeFcn(handles.figure1, [], handles)
    guidata(fig, handles);
    
    if nargout > 0
        varargout{1} = fig;
    end
    
elseif ischar(varargin{1}) % INVOKE NAMED SUBFUNCTION OR CALLBACK
    
    try
        %     [varargout{1:nargout}] = 
        feval(varargin{:}); % FEVAL switchyard
    catch
        disp(lasterr);
        fig = openfig(mfilename,'reuse');
        handles = guihandles(fig);
%         disperr(handles);
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

function FileUpdate(h, handles)

function figure1_CloseRequestFcn(varargin)
closereq;

function Figure_ResizeFcn(h, ev, handles)
main_frame = get(handles.figure1, 'Position');
but_h = 30;
brd = 5;
butPos = [brd, brd, main_frame(3)-2*brd, but_h];
set(handles.butCalc, 'Position', butPos);
frPos = [brd, brd*2+but_h, main_frame(3)-2*brd, main_frame(4)-3*brd-but_h];
set(handles.frame1, 'Position', frPos);
eParPos = [frPos(1) + brd, frPos(2)+brd, frPos(3)-2*brd, 20];
set(handles.ePars, 'Position', eParPos);
slPos = [frPos(1) + brd, sum(eParPos([2, 4]))+brd, 30, 20];
set(handles.slPars, 'Position', slPos);
popPos = [frPos(1) + 2*brd+30, sum(eParPos([2, 4]))+brd, frPos(3) - 3*brd-30, 20];
set(handles.popup, 'Position', popPos);
set(handles.list, 'Position', [frPos(1) + brd, sum(slPos([2, 4]))+brd,...
        frPos(3)-2*brd, frPos(4)-4*brd - slPos(4) - eParPos(4)]);

% --------------------------------------------------------------------
function varargout = list_Callback(h, handles)
handles = guidata(handles.figure1);
Res = get(handles.list, 'String');
val = get(handles.list, 'Value');

[name, str1] = strtok(Res{val}, '=');
str2 = [strtrim(str1(2:end)), ' '];

if (str2(1)=='''')
    primes = findstr(str2, '''');
    if size(primes, 2) < 2, primes(2) = size(str2, 2); end
    set(handles.ePars, 'String', str2(2:(primes(2)-1)));
    set(handles.ePars, 'UserData', 'string');
    set(handles.slPars, 'Enable', 'off');    
    set(handles.popup, 'Enable', 'off');    
else
    dig = str2num(str2);
    set(handles.ePars, 'String', str2);
    set(handles.ePars, 'UserData', 'value');
    set(handles.slPars, 'Enable', 'on');
    set(handles.popup, 'Enable', 'on');    
end

function varargout = butCalc_Callback(h, eventdata, handles, varargin)
handles.operate = get(handles.butCalc, 'Value');
guidata(handles.figure1, handles);

if handles.operate
    ePars_Callback(handles.ePars, [], handles);
end

% --------------------------------------------------------------------
function varargout = ePars_Callback(h, eventdata, handles, varargin)
% Stub for Callback of the uicontrol handles.ePars.
handles = guidata(handles.figure1);
Res = get(handles.list, 'String');
val = get(handles.list, 'Value');
newval = strtrim(get(handles.ePars, 'String'));

[name, str1] = strtok(Res{val}, '=');
name = strtrim(name);

switch get(handles.ePars, 'UserData')
    case 'string'
        set(handles.ePars, 'String', newval);
        Res{val} = [name ' = ''', newval,''''];
    otherwise
        nums = str2num(newval);
        set(handles.ePars, 'String', strtrim(newval));
        Res{val} = [name ' = ' strtrim(newval)];
end
set(handles.list, 'String', Res);
list_Callback(h, handles);
handles.sim.Script = Res;

process_script(handles);

% --------------------------------------------------------------------
function varargout = slPars_Callback(h, eventdata, handles, varargin)
handles = guidata(handles.figure1);
if ~strcmp(get(handles.ePars, 'UserData'),'value'), return; end;
shift = get(h, 'Value');
set(h, 'Value', 0);
CurVal = str2num(get(handles.ePars, 'String'));
num = get(handles.popup, 'Value');
str = get(handles.popup, 'String');
dim = str2num([str{num}]);
Val = CurVal + dim*shift;

set(handles.ePars, 'String', num2str(Val, 15));
ePars_Callback(h, eventdata, handles, varargin);

function plothis(handles)
handles = guidata(handles.figure1);
% stat = strcmp(get(handles.mFixAxis, 'Checked'), 'on');

hand = guidata(handles.hh);

hand.out = {};
ax = hand.src.ax;

c = {'off', 'on'};
hipx =0;  hipy = 0;
errr = 0;

hand.out{1}.ax = ax;
hand.out{1}.y = hand.src.y;
if(isfield(handles,'x_a') && isfield(handles,'y_a'))
    isz_x = size(hand.src.y,2);
    isz_y = size(hand.src.y,1);

    hand.out{1}.ax.x = [1:isz_x]'*handles.x_a + handles.x_b;
    hand.out{1}.ax.y = [1:isz_y]'*handles.y_a + handles.y_b;
else
    hand.out{1}.ax.x = hand.src.ax.x;
    hand.out{1}.ax.y = hand.src.ax.y;
end
hand.out{1}.ax.c = [0, 0, 1];
hand.out{1}.ax.s = 0;
hand.out{1}.ax.type = 'data';
hand.out{1}.xlabel = ax.xlabel;
hand.out{1}.forceplot = 'image';
hand.out{1}.title = 'Src';

if(isfield(handles,'Dimensions'))
    hand.out{end+1}.ax = [];
    hand.out{end}.y = handles.Dimensions(2)*[1,1];
    hand.out{end}.ax.x = handles.Dimensions([1,3]);
    hand.out{end}.ax.y = hand.out{end}.y;
    hand.out{end}.ax.Color = [0,1,1];
    hand.out{end}.ax.type = 'data';
    hand.out{end}.xlabel = ax.xlabel;
    hand.out{end}.forceplot = 'line';
    hand.out{end}.title = 'Dims1';
    hand.out{end}.ax.LineStyle = '-';
    hand.out{end}.ax.LineWidth = 1;
    hand.out{end}.ax.Marker = 'none';
    
    hand.out{end+1}.ax = [];
    hand.out{end}.y = handles.Dimensions([2,4]);
    hand.out{end}.ax.x = handles.Dimensions(1)*[1,1];
    hand.out{end}.ax.y = hand.out{end}.y;
    hand.out{end}.ax.Color = [0,1,1];
    hand.out{end}.ax.type = 'data';
    hand.out{end}.xlabel = ax.xlabel;
    hand.out{end}.forceplot = 'line';
    hand.out{end}.title = 'Dims2';
    hand.out{end}.ax.LineStyle = '-';
    hand.out{end}.ax.LineWidth = 1;
    hand.out{end}.ax.Marker = 'none';
end

if(isfield(handles,'Region'))
    hand.out{end+1}.ax = ax;
    hand.out{end}.y = [handles.Region(2), handles.Region(2), handles.Region(4), handles.Region(4), handles.Region(2)];
    hand.out{end}.y = hand.out{end}.y*handles.y_a + handles.y_b;
    hand.out{end}.ax.x = [handles.Region(1), handles.Region(3), handles.Region(3), handles.Region(1), handles.Region(1)];
    hand.out{end}.ax.x = hand.out{end}.ax.x*handles.x_a + handles.x_b; 
    hand.out{end}.ax.y = hand.out{end}.y;
    hand.out{end}.ax.Color = [0.5, 0.5, 0.5]*0;
    hand.out{end}.ax.type = 'data';
    hand.out{end}.xlabel = ax.xlabel;
    hand.out{end}.forceplot = 'line';
    hand.out{end}.title = 'Borders';
    hand.out{end}.ax.LineStyle = ':';
    hand.out{end}.ax.LineWidth = 1;
    hand.out{end}.ax.Marker = '.';
end

if(isfield(handles,'yy'))
    hand.out{end+1}.ax = ax;
    hand.out{end}.y = handles.yy;
    hand.out{end}.ax.x = handles.xx;
    hand.out{end}.ax.y = hand.out{end}.y;
    hand.out{end}.ax.Color = [0, 0, 1];
    hand.out{end}.ax.LineStyle = '-';
    hand.out{end}.ax.LineWidth = 1;
    hand.out{end}.ax.Marker = 'none';
    hand.out{end}.ax.type = 'data';
    hand.out{end}.xlabel = ax.xlabel;
    hand.out{end}.forceplot = 'line';
    hand.out{end}.title = 'Result';
end

if ~isempty(handles.exclude)
    for ci = 1:size(handles.exclude, 1)
        hand.out{end+1}.ax = ax;
        hand.out{end}.y = [handles.exclude(ci,2), ...
                handles.exclude(ci,2), handles.exclude(ci,4), ...
                handles.exclude(ci,4), handles.exclude(ci,2)];
        hand.out{end}.y = hand.out{end}.y*handles.y_a + handles.y_b;
        hand.out{end}.ax.x = [handles.exclude(ci,1), ...
                handles.exclude(ci,3), handles.exclude(ci,3), ...
                handles.exclude(ci,1), handles.exclude(ci,1)];
        hand.out{end}.ax.x = hand.out{end}.ax.x*handles.x_a + handles.x_b; 
        hand.out{end}.ax.y = hand.out{end}.y;
        hand.out{end}.ax.Color = [1, 0.05, 0.05];
        hand.out{end}.ax.LineStyle = ':';
        hand.out{end}.ax.LineWidth = 1;
        hand.out{end}.ax.Marker = 'none';
        hand.out{end}.ax.type = 'data';
        hand.out{end}.xlabel = ax.xlabel;
        hand.out{end}.forceplot = 'line';
        hand.out{end}.title = 'exclude';
    end
end

hand.PlotType = 5;
guidata(handles.hh, hand);
[fpath,name,ext,ver] = fileparts(get(handles.hh, 'FileName'));
eval([name '(''SetDataSource'', 2, hand)']);

% --------------------------------------------------------------------
function varargout = Eraser_Callback(h, eventdata, handles, varargin)
hand = guidata(handles.hh);
[path,name] = fileparts(get(handles.hh, 'FileName'));

[x,y]=eval([name '(''GetBox'', hand)']);
if x(1)==x(2) && y(1)==y(2), return; end
x = sort(x); y = sort(y);

if hand.DataSource == 2
    left = (x(1) - handles.x_b)/handles.x_a;
    bottom   = (y(1)- handles.y_b)/handles.y_a;
    right = (x(2) - handles.x_b)/handles.x_a;
    top   = (y(2) - handles.y_b)/handles.y_a;
else
    left = x;
    bottom = y;
    right = x;
    top = y;
end

handles.exclude(end+1, :) = [left, bottom, right, top];
process_script(handles);

% --------------------------------------------------------------------
function varargout = KillEraser_Callback(h, eventdata, handles, varargin)
if isempty(handles.exclude), return; end

hand = guidata(handles.hh);
[path,name] = fileparts(get(handles.hh, 'FileName'));

[x,y]=eval([name '(''GetPoint'', hand, 1000)']);

if hand.DataSource == 2
    left = (x - handles.x_b)/handles.x_a;
    bottom   = (y- handles.y_b)/handles.y_a;
else
    left = x;
    bottom = y;
end

exclude = handles.exclude;
handles.exclude = []; 

for jj = 1:size(exclude, 1)
    for ii = 1:length(x)
        % if exclude(1)<tx<exclude(2), then inx = 1, else 0;
        inx(ii) = ~floor((left(ii) - exclude(jj, 1))/diff(exclude(jj, [1, 3])));
        iny(ii) = ~floor((bottom(ii) - exclude(jj, 2))/diff(exclude(jj, [2, 4])));
    end
    ind = sum(inx)*sum(iny);
    if ~ind
        handles.exclude(end+1, :) = exclude(jj, :);
    end
end

process_script(handles);

function varargout = MainFigure_ButtonDown(h, eventdata, handles)

% --------------------------------------------------------------------
function varargout = SelReg_Callback(h, eventdata, handles, varargin)
hand = guidata(handles.hh);
[path,name,ext,ver] = fileparts(get(handles.hh, 'FileName'));

[x,y]=eval([name '(''GetBox'', hand)']);
if x(1)==x(2)&y(1)==y(2), return; end
x = sort(x); y = sort(y);

if hand.DataSource == 2
    left = (x(1) - handles.x_b)/handles.x_a;
    bottom   = (y(1)- handles.y_b)/handles.y_a;
    right = (x(2) - handles.x_b)/handles.x_a;
    top   = (y(2) - handles.y_b)/handles.y_a;
else
    left = x;
    bottom = y;
    right = x;
    top = y;
end

SetParameter(handles, 'Region_left',num2str(left));
SetParameter(handles, 'Region_top',num2str(top));
SetParameter(handles, 'Region_right',num2str(right));
SetParameter(handles, 'Region_bottom',num2str(bottom));

process_script(handles);

% --------------------------------------------------------------------
function handles = process_script(handles)

hand = guidata(handles.hh);

Scr = get(handles.list, 'String');
for ii = 1:length(Scr)
    eval([Scr{ii}, ';']);
end

isz_x = size(hand.src.y,2);
isz_y = size(hand.src.y,1);

handles.Dimensions  = [First_Xvalue, First_Yvalue, Last_Xvalue, Last_Yvalue];
handles.Region = floor([Region_left, Region_bottom, Region_right, Region_top]);

handles.x_a    = (Last_Xvalue - First_Xvalue)/(Last_Xpoint - First_Xpoint);
handles.x_b    = First_Xvalue-handles.x_a*First_Xpoint;

handles.y_a    = (Last_Yvalue - First_Yvalue)/(Last_Ypoint - First_Ypoint);
handles.y_b    = First_Yvalue-handles.y_a*First_Ypoint;

if handles.operate, 
    % Clip the image 
    yy = hand.src.y(min(handles.Region([2,4])):max(handles.Region([2,4])),...
        min(handles.Region([1,3])):max(handles.Region([1,3])), :);
    
    % Bring to color format
    switch Selected_color
        case {'r', 'g', 'b'}
            if size(yy, 3)==3
                strs = {'r', 'g', 'b'};
                ind = find(strcmp(Selected_color, strs));
                yy = double(yy(:, :, ind));
            else
                yy = double(yy(:, :, 1));
            end
        otherwise
            % Sum over all colors (if any)
            yy = sum(yy, 3);
    end
    yy = 1 - yy/max(yy(:));  % 0 - data, 1 - background
    
    % Apply Erasers
    zy = ones(size(yy));
    xsz = size(yy, 2); ysz = size(yy, 1);
    if ~isempty(handles.exclude)
        for ii = 1:size(handles.exclude, 1)
            %             %         kx = nearme(hand.src.ax.x, handles.exclude(ii, 1));
            %             %         ky = nearme(hand.src.ax.y, handles.exclude(ii, 2));
            aa = floor(sort(handles.exclude(ii, [1, 3])) - Region_left);
            aa(aa < 1) = 1; aa(aa > xsz) = xsz;
            bb = floor(sort(handles.exclude(ii, [2, 4])) - Region_top);
            bb(bb < 1) = 1; bb(bb > ysz) = ysz;
            kx  = aa(1); ky  = bb(1);
            kx1 = aa(2); ky1 = bb(2);  
            
            zy(ky:ky1, kx:kx1) = 0;
        end
    end
    yy(zy == 0) = 0;

    % Get Data
    YY = zeros(xsz, 1);
    for cc = 1:xsz
        ty = yy(:, cc);
        ind = find(ty < Lower_color_level)-2;
        if ~isempty(ind)
            av_ind = sum(ind)/length(ind);
            YY(cc) = (av_ind + Region_top) * handles.y_a + handles.y_b;
        end
    end
    
    XX = linspace(Region_left*handles.x_a + handles.x_b, ...
        Region_right*handles.x_a + handles.x_b, xsz).';
    
    if nPoints > 1, 
        handles.xx = linspace(Region_left*handles.x_a + handles.x_b, ...
            Region_right*handles.x_a + handles.x_b, nPoints).';
        
        handles.yy = spline(XX, YY', handles.xx')';
    else
        handles.xx = XX;
        handles.yy = YY;
    end
end
guidata(handles.figure1, handles);
plothis(handles);

function varargout = SelLeftBottom_Callback(h, eventdata, handles, varargin)
hand = guidata(handles.hh);
[path,name] = fileparts(get(handles.hh, 'FileName'));

[x,y]=eval([name '(''GetPoint'', hand, 1)']);

if hand.DataSource == 2
    left = (x - handles.x_b)/handles.x_a;
    bottom   = (y- handles.y_b)/handles.y_a;
else
    left = x;
    bottom = y;
end

SetParameter(handles, 'First_Xpoint',num2str(left));
SetParameter(handles, 'First_Ypoint',num2str(bottom));

process_script(handles);

% --------------------------------------------------------------------
function varargout = SelRightTop_Callback(h, eventdata, handles, varargin)
hand = guidata(handles.hh);
[path,name] = fileparts(get(handles.hh, 'FileName'));

[x,y]=eval([name '(''GetPoint'', hand, 1)']);

if hand.DataSource == 2
    right = (x - handles.x_b)/handles.x_a;
    top   = (y- handles.y_b)/handles.y_a;
else
    right = x;
    top = y;
end

SetParameter(handles, 'Last_Xpoint',num2str(right));
SetParameter(handles, 'Last_Ypoint',num2str(top));

process_script(handles);

% --------------------------------------------------------------------
function SetParameter(handles, Field, Value)

str = get(handles.list, 'String');
for ii = 1:length(str)
    [name, str1] = strtok(str{ii}, '=');
    nme = strtrim(name(1:end));
    if strcmp(nme,Field)
            str{ii} = [nme, ' = ', Value];
    end
end
set(handles.list, 'String', str);

function mAbout_Callback(h, eventdata, handles, varargin)
msgbox({'Image 2 data conversion';'Version 2.0 (07.12.2005)'; ...
        'by Alexey Silakov and Boris Epel';' ';
        'Step 1: Define correct scale of the image by selecting';
        '        Dim (L)eft(B)ottom and Dim (R)ight(T)op corners';
        '        and specifying First/Last X/Y value';
        'Step 2: Select region of the image that will be processed';
        'Step 3: Remove not relevant areas of the image using eraser.';
        'Press Calculate'
           }, ...
    'About', 'help')