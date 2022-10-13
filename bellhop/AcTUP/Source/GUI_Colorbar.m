function h = GUI_Colorbar(Label)
%h = GUI_Colorbar(Label)

h = colorbar;
if nargin >= 1
	h2 = get(h, 'ylabel');
	set(h2, 'string', Label);
end

