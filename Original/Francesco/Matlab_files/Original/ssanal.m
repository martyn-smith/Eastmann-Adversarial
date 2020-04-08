%                        ***** ssanal.m *****
%
% gt rows are 1:ny physical variables in control structure 
%             ny+1:ny+nc noisy variables for use in control
% gt columns  1:nu are the manipulated inputs,
%             nu+1 to nu+nd are the disturbances
%             nu+nd+1 to nu+nd+nsp are setpoint changes
%             nu+nd+nsp+1 to nu+nd+nsp+nc are the measurement biases
% gt2 is the same except scaled for analysis of weak connections
%
format short e
%
%  analyse for weak connections
%
[U1,S1,V1]=svd(gt2(ny+1:ny+nc,1:nu));
for i=1:nc
   s1(i)=S1(i,i);
   if s1(i)>1/sqrt(nu) Nfe=i; end
   if s1(i)>1.e-10 Nf=i; end
end
disp([' Number of effective degrees of freedom is: ',num2str(Nfe)])
disp([' Number of degrees of freedom is: ',num2str(Nf)])
iweak=0;
if s1(nc)<.5/nu
   iweak=2;
   disp('Structure is flawed');
elseif s1(nc) <1/sqrt(nu)
   iweak=1;
   disp('Structure is potentially weak'); 
end
%
% print analysis of singular vectors only at print level 2 or above
%
if iprint>1
   disp(' ')
      disp(' ')
      disp('      Analysis of ineffective degrees of freedom')
      disp(' ')
   %
   % check for weak inputs%
   %
   % identify number of inputs contributing substantially to the weakest
   % direction
   %
      v=V1(:,nc);
      vmax=max(abs(v));
      clear indu
      count=0;
      for i=1:nu
          if abs(v(i))> vmax/10.
             count=count+1;
             indu(count)=i;
          end
          if abs(v(i))==vmax
             indmax=i;
          end
      end
      if count==1 
         disp('Possible weak manipulated variable  ',num2str(iu(indmax)))
      elseif count<=3
         disp('Possible dependent manipulated variables')
         for i=1:count
             disp(num2str(iu(indu(i))))
         end
         disp (' ')
      end
   %
   % check for weak outputs
   %
   % identify number of outputs contributing substantially to the weakest
   % direction
   %
      v=U1(:,nc);
      vmax=max(abs(v));
      clear indy
      count=0;
      for i=1:nc
          if abs(v(i))> vmax/10.
                count=count+1;
             indy(count)=i;
          end
          if abs(v(i))==vmax
             indmax=i;
          end
      end
      if count<4
        disp (' The output numbers relate to the original transfer function ')
      end 
      if count==1 
        disp(['Possible insensitive measured variable  ',num2str(ic(indmax))])
      elseif count<=3
         disp('Possible dependent measured variables')
         for i=1:count
            disp(num2str(ic(indy(i))))
         end
      end
   end
%
% calculate single input effectiveness if excess inputs exist
%
disp (' ')
if nc < nu
   disp ('        Single input effectiveness                 ')
   disp (' ')
   disp ('Inputs with effectiveness close to 1 are preferable')
   disp ('This result is independent of *output* scaling')
   SIE=sum((V1(:,1:Nf).*V1(:,1:Nf))');
   disp(' ')
   disp('Input     SIE')
   for i=1:nu
      disp([num2str(iu(i)),'    ',num2str(SIE(i))])
   end
   disp(' ')
end
%
% calculate scaled single input effectiveness if ineffective degrees of freedom% exist
%
disp('')
if Nfe<Nf
   disp ('     Scaled single input effectiveness                 ')
   disp (' ')
   disp ('Inputs with effectiveness close to 1 are preferable')
   SSIE=sum((V1(:,1:Nfe).*V1(:,1:Nfe))');
   disp('This result depends on the specified ranges for y')
   disp(' ')
   disp('Input     SSIE')
   for i=1:nu
      disp([num2str(iu(i)),'    ',num2str(SSIE(i))])
   end
   disp(' ')
   disp ('Outputs with effectiveness close to 1 are preferable')
   disp ('This result depends on the specified ranges for y')
   SSOE=sum((U1(:,1:Nfe).*U1(:,1:Nfe))');
   disp(' ')
   disp('Output     SSOE')
   for i=1:nc
      disp([num2str(ic(i)),'    ',num2str(SSOE(i))])
   end
   disp(' ')
end
disp('')
%
% analyse for minimised condition number
%
gc=gt(ny+1:ny+nc,1:nu);
if iprint>=1
   rga=gc.*pinv(gc)';
   mecn=max(norm(rga,1),norm(rga,inf));
   mecn=mecn+sqrt(mecn*mecn-1);
   disp(' ')
   disp ('     Minimised Euclidean condition number          ')
   disp (['              ', num2str(mecn),'      '])
   disp ('Values close to 1 are preferable')
   disp ('Values above 50 are quite undesirable ')
   disp('  ')
end
%
%     compute perfect control gains
%
% manipulated variables should be normalised by their range for this
% calculation (only matters if the number of manipulated variables is
% greater than the  number of controlled variables)
% so that pseudo-inverse attaches a balanced significance to each input--
% minimising the sum of squares of variation of the manipulated variables as
% a fraction of their range
%
clear gcp igc
for i=1:nu
    gcp(:,i)=gc(:,i)*(uht(i)-ult(i));
end
%
igcp=pinv(gcp);
% 
for i=1:nu
   igc(i,:)=igcp(i,:)*(uht(i)-ult(i));
end

%   gud is the gain from the disturbance variables to the selected
%   manipulated variables (in iu)
gud=-igc*gt(ny+1:ny+nc,nu+1:nt);
%   gd is the gain from the disturbance variables to the measured
%   variables
gd=gt(:,nu+1:nt) + gt(:,1:nu)*gud;
%
% Check if outputs vary excessively 
%
ymax=ysst;
ymin=ysst;
for i=1:ny
   for j=1:nd+nsp+nc
      if gd(i,j) > 0
         ymax(i)=ymax(i)+gd(i,j)*(uht(j+nu)-usst(j+nu));      
         ymin(i)=ymin(i)+gd(i,j)*(ult(j+nu)-usst(j+nu));
      else
         ymax(i)=ymax(i)+gd(i,j)*(ult(j+nu)-usst(j+nu));
         ymin(i)=ymin(i)+gd(i,j)*(uht(j+nu)-usst(j+nu));
      end
   end
end
%
%  check if input variation is excessive
%
umax=usst;
umin=usst;
for i=1:nu
   for j=1:nd+nsp+nc
      if gud(i,j) > 0
         umax(i)=umax(i)+gud(i,j)*(uht(j+nu)-usst(j+nu));      
         umin(i)=umin(i)+gud(i,j)*(ult(j+nu)-usst(j+nu));
       else
         umax(i)=umax(i)+gud(i,j)*(ult(j+nu)-usst(j+nu));
         umin(i)=umin(i)+gud(i,j)*(uht(j+nu)-usst(j+nu));
      end
   end
end
%
% scan output deviations for violations and display
%
clear mvarprcy
for i=1:ny
   mvarprcy(i)=100*max(ymax(i)-yht(i),ylt(i)-ymin(i))/(yht(i)-ylt(i));
end
if (max(mvarprcy))>0
   disp('            Output Variation excessive') 
   disp(' ') 
end
if max(mvarprcy)>0 |iprint>=1
   disp('              Output deviation analysis          ')
   disp(' ')
   disp('Element |  Percentage | lower | upper | minimum| maximum |')
   disp(' of y   |  violation  | limit | limit | value  | value   |')
   disp(' ')
end
for i=1:ny
   if mvarprcy(i) > 0 | iprint>=1
      disp([num2str(i),'      ',num2str(mvarprcy(i)),'      ',...
      num2str(ylt(i)),'      ',num2str(yht(i)),'      ', ...
         num2str(ymin(i)),'      ',num2str(ymax(i))])
   end
end
disp(' ')
%
% scan input deviations for violations and display
%
clear mvarprcu
for i=1:nu
   mvarprcu(i)=100*max(umax(i)-uht(i),ult(i)-umin(i))/(uht(i)-ult(i));
end
if max(mvarprcu)>0 
   disp ('               Input variation excessive') 
   disp(' ')
end
if max(mvarprcu)>0 |iprint>=1
   disp('              Input deviation analysis          ')
   disp(' ')
   disp('Element |  Percentage | lower | upper | minimum| maximum |')
   disp(' of u   |  violation  | limit | limit | value  | value   |')
   disp(' ')
end
for i=1:nu
   if mvarprcu(i) > 0 | iprint>=1
      disp([num2str(iu(i)),'      ',num2str(mvarprcu(i)),'      ',...
      num2str(ult(i)),'      ',...
      num2str(uht(i)),'      ', num2str(umin(i)),'      ',num2str(umax(i))])
   end
end
%
disp(' ')
%
% Carry out RGA/NI analysis on elements outside stabilising structure
%
gu=gt([ic],1:nu);
gs=gt2([ic],1:nu);
gsd=gt2([ic],nu+1:nt);
if nus>0
  ic1=ic(1:nus);
  ic2=ic(nus+1:nc);
  gcs=gt([ic1],1:nus);
  igc1=inv(gcs);
  gu1=-igc1*gt([ic1],nus+1:nu);
  gsubu=gt([ic2],nus+1:nu) + gt([ic2],1:nus)*gu1;
  gcs=gt2([ic1],1:nus);
  igc1=inv(gcs);
  gu1=-igc1*gt2([ic1],nus+1:nu);
  gu1d=-igc1*gt2([ic1],nu+1:nt);
  gsubs=gt2([ic2],nus+1:nu) + gt2([ic2],1:nus)*gu1;
  gsubsd=gt2([ic2],nu+1:nt) + gt2([ic2],1:nus)*gu1d;
else
  gsubs=gs;
  gsubu=gu;
  gsubsd=gsd;
end
%
rga=gsubs.*pinv(gsubs')
% if rga is satisfactory this test provides an additional check on integrity
if nu == nc
   nid=det(gsubs);
   for i=1:nu-nus
       nid=nid/gsubs(i,i);
   end
   disp 'NI for diagonal pairing'
   disp(['   ',num2str(nid)])
end
%
% analyse diagonal dominance
%
if iprint >= 2
 disp('***Warning the tests below can be misleading***')
 disp ' '
 disp 'Testing if paired input dominates other inputs for corresponding output'
 disp '1 equals complete dominance, 0 equals negligible effect'
 disp 'values much less than 1/nu are undesirable as interactions'
 disp 'may be significant '
 disp ' '
 disp 'overall control structure'
 ds=sum(abs(gs'));
 for i=1:nc
   disp(['Loop  ',num2str(i),' ', num2str(abs(gs(i,i))/ds(i)) ])
 end
 disp ' '
 disp 'non-core loops'
 ds=sum(abs(gsubs'));
 for i=1:nc-nus
   disp(['Loop  ',num2str(i+nus),' ', num2str(abs(gsubs(i,i))/ds(i)) ])
 end
 %
 % analyse ability to cancel disturbances locally
 %
 disp ' '
 disp 'Testing if paired input dominates disturbances for corresponding output'
 disp 'the bigger the better '
 disp 'values much less than 1 are undesirable as the loop will be unable '
 disp 'to bring the output under control without help from the other loops'
 disp 'this measure is particularly important for the faster loops'
 disp ' '
 disp 'overall control structure'
 ds=sum(abs(gsd'));
 for i=1:nc
    disp(['Loop  ',num2str(i),' ', num2str(abs(gs(i,i))/ds(i)) ])
 end
 disp ' '
 disp 'non-core loops'
 ds=sum(abs(gsubsd'));
 for i=1:nc-nus
    disp(['Loop  ',num2str(i+nus),' ', num2str(abs(gsubs(i,i))/ds(i)) ])
 end
end





