function xdot = f(x,t)
# SNR = [-10:5:30]; %in Db
  snr = 130;

  Dc = 1.0;
  h0 = 10^9;
  p = 0.4; # Population Density of Ho
  k1 = 0.4;
#k2 = awgn(0.4,snr,'measured');
  k2 = 0.4;
  k3 = 6.1;
  k4 = 0.2;
  kg = 0.5;

  inhibit = 0.9;
  kgp = kg*inhibit;
  kgpp = kgp*inhibit;
  kgppp = kgpp*inhibit;
    
% Each successive growth rate (slightly) inhibited

xdot(1) = Dc*h0 - Dc*x(1) - k1*p*x(1) + kg*x(1);


xdot(2) = -Dc*x(2) + k1*x(1) + kgp*x(2) - k2*x(2);


xdot(3) = -Dc*x(3) + kgpp*x(3) + k2*x(2) - k4*x(3);


xdot(4) = -Dc*x(4) + kgppp*x(4) + k4*x(3);


xdot(5) = -Dc*x(5) - k1*p*x(1) + k3*x(3);


endfunction

function colortable()
co = get(gca,'ColorOrder');
fd = fopen('ctable.m','wt');
fprintf(fd,'mycolors = [\n');
fprintf(fd,'   %f,%f,%f;\n',co);
fprintf(fd,'];\n');
fclose(fd);
endfunction

#colortable(); # Generate a template color table like this:
mycolors = [
   [0.000000 0.000000 1.000000]; # Blue Devil
   [0.000000 0.750000 0.750000]; # Cyan
   [0.250000 0.000000 0.500000]; # Pink
   [0.000000 0.750000 0.000000]; # Green
   [0.000000 0.000000 0.750000]; # Carolina Blue
   [0.750000 0.000000 0.250000]  # Purple
];
hostcolor =  [0.00 0.00 1.00];  % Blue Devil Blue
host2color = [0.00 0.75 0.00];  % Green
host3color = [0.75 0.00 0.00];  % Red
phagecolor = [0.75 0.00 0.75];  % Purple
zcolor =     [0.00 0.25 0.75];  % Carolina Blue

# Initial conditions x(1)=1000 and x(2..5)=0 on [0,20] with 200 points:

x = lsode("f", [1000;0;0;0;0], (t = linspace (0,20,200)' ));
plot(t,x);
o = x(120,1)*0.05;
text(12,x(120,1)+o,'Uninfected Host','fontsize',22,'color',hostcolor);
text(12,x(120,2)+o,'Adsorbed Host','fontsize',24,'color',host2color);
text(12,x(120,3)+o,'Productive Host','fontsize',28,'color',host3color);
text(12,x(120,4)+o,'Z','fontsize',28,'color',zcolor);
text(12,x(120,5)+o,'Phage','fontsize',32,'color',phagecolor);


