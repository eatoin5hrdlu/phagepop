pkg load image
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.75 0.00 0.75];  % Purple
zcolor =     [0.00 0.25 0.75];  % Carolina Blue
global kon;
global Dc;
global h0;
 load ES_params;  % Loads the global variables

%  Initial conditions x(1)=1000 and x(2..5)=0 on [0,20] with 200 points:
x = lsode("f", [1000;0;0;0;0], (t = linspace (0,20,200)' ));
y = lsode("g", [1000;0;0;0;0], (u = linspace (0,20,200)' ));
fhandle = figure(1);
plot(t,x);
hold on;
plot(t,y);


[fh,msg] = jpfill(t,x(:,1), y(:,1),hostcolor);
[fh,msg] = jpfill(t,x(:,2), y(:,2),host2color);
[fh,msg] = jpfill(t,x(:,3), y(:,3),host3color);
[fh,msg] = jpfill(t,x(:,4), y(:,4),zcolor);
[fh,msg] = jpfill(t,x(:,5), y(:,5),phagecolor);

o = x(120,1)*0.13;
text(10,x(120,1)-o,'Uninfected Host','fontsize',22,'color',hostcolor);
text(10,x(120,2)+o,'Adsorbed Host','fontsize',24,'color',host2color);
text( 8,x(120,3)+o,'Productive Host','fontsize',28,'color',host3color);
text(10,x(120,4)-o,'Z','fontsize',28,'color',zcolor);
text(10,x(120,5)+o,'Phage','fontsize',32,'color','b');

print(fhandle,'-dpng','-color','phagepop.png');

