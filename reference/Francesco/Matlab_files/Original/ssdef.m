%                        ***** ssdef.m *****
%
% default controllability analysis
%
spl=-.1*abs(yss([isp]));
sph=.1*abs(yss([isp]));
[ny,nu]=size(g);
for i=1:ny
   if yss(i)>0
      yh(i)=max(1.e6*yss(i),1.);
      yl(i)=0;
   else
      yh(i)=max(-1.e6*yss(i),1.);
      yl(i)=-max(-1.e6*yss(i),1.);
   end
end
for i=1:nu
   if uss(i)>0
      uh(i)=max(1.e6*uss(i),1.);
      ul(i)=0;
   else
      uh(i)=max(-1.e6*uss(i),1.);
      ul(i)=-max(-1.e6*uss(i),1.);
   end
end
%
% default measurement bias
%
bias=max(frac_noise*abs(yss),1.e-10);
rangey=max(frac_range*abs(yss),1.e-10);
