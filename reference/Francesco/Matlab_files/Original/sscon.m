%                            ***** sscon.m ******
%
% Check problem consistency
%
[dummy,nsp]=size(isp);
[dummy,nd]=size(id);
[dummy,nc]=size(ic);
[dummy,nu]=size(iu);
iflag=1;
if nc > nu
   disp (' Ic must not be longer than iu' )
   iflag=0;
end
for i=1:nsp
   ispin=0;
   for j=1:nc
       if isp(i)==ic(j) ispin=1; end
   end
   if ispin == 0
      disp (' All setpoints must be controlled' )
      iflag=0;
   end
end
for i=1:ny
   if yl(i)>= yh(i)
      disp ('upper limit less than or equal to lower limit' )
      disp ('output') 
      i
      iflag=0;
   end
   if yss(i)>yh(i) | yss(i)<yl(i)
      disp ('steady state output outside limits ' )
      disp ('output') 
      i
      iflag=0;
   end	
end
for i=1:nu
   if ul(i)>= uh(i)
      disp ('upper limit less than or equal to lower limit' )
      disp ('input')
      i
      iflag=0;
   end
   if uss(i)>uh(i) | uss(i)<ul(i)
      disp ('steady state input outside limits ' )
      disp ('input') 
      i
      iflag=0;
   end	
end
for i=1:nc
   if yss(ic(i))+bias(ic(i))>yh(ic(i)) | yss(ic(i))-bias(ic(i))<yl(ic(i))
      disp ('bounds are inconsistent with measurement biases ' )
      disp ('output') 
      ic(i)
      iflag=0;
   end	
end
if iflag==0 
   break; 
end
%
[dummy,nsp]=size(isp);
nt=nu+nd+nsp+nc;
%
% Extract temporary g matrix with manipulated variables and
% disturbances extracted and reordered.
%
gt=g(:,[[iu],[id]]);
%
% Augment problem with setpoint disturbances
%
gt=[gt zeros(ny,nsp)];
ult=ul([[iu],[id]]);
uht=uh([[iu],[id]]);
usst=uss([[iu],[id]]);
for i=1:nsp
%  setpoint changes are analysed as disturbances in the opposite direction.
   gt(isp(i),nu+nd+i)=-1.d0;
   ult(nu+nd+i)=spl(i);
   uht(nu+nd+i)=sph(i);
   usst(nu+nd+i)=0;
end
%
% Augment problem with nc noisy controlled variables and noise disturbances
%
ysst=[yss yss([ic])];
ylt=[yl yl([ic])];
yht=[yh yh([ic])];
gt=[gt zeros(ny,nc);gt([ic],:) zeros(nc,nc)];
%
for i=1:nc
   gt(ny+i,nu+nd+nsp+i)=1.d0;
   ult(nu+nd+nsp+i)=-bias(ic(i));
   uht(nu+nd+nsp+i)=bias(ic(i));
   usst(nu+nd+nsp+i)=0;
end
rangeu=uht-ult;
%
% generate a scaled gt for analysis of weak effects.
%
gt2=inv(diag([[rangey] [rangey([ic])]]))*gt*diag([rangeu]);







