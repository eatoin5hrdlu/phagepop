set(gca,'LineWidth',1.0);
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.75 0.00 0.75];  % Purple
zcolor =     [0.00 0.25 0.75];  % Carolina Blue

# Initial conditions x(1)=1000 and x(2..5)=0 on [0,20] with 200 points:
x = lsode("f", [1000;0;0;0;0], (t = linspace (0,20,200)' ));
plot(t,x);
hold on;

o = x(120,1)*0.05;
text(12,x(120,1)+o,'Uninfected Host','fontsize',22,'color',hostcolor);
text(12,x(120,2)+o,'Adsorbed Host','fontsize',24,'color',host2color);
text(12,x(120,3)+o,'Productive Host','fontsize',28,'color',host3color);
text(12,x(120,4)+o,'Z','fontsize',28,'color',zcolor);
text(12,x(120,5)+o,'Phage','fontsize',32,'color',phagecolor);

[fh,msg] = jpfill(t,x(:,2), x(:,3));


