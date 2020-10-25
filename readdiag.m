idm=182;jdm=142;

fid=fopen('fort.11','r','b');
[i count]=fread(fid,1,'int32');
n=0;
while count~=0
  n=n+1;
  ub(:,:,n)=reshape(fread(fid,idm*jdm,'float64'),idm,jdm);
  fread(fid,1,'int32');
  [i count]=fread(fid,1,'int32');
end
fclose(fid);

fid=fopen('fort.12','r','b');
[i count]=fread(fid,1,'int32');
n=0;
while count~=0
  n=n+1;
  vb(:,:,n)=reshape(fread(fid,idm*jdm,'float64'),idm,jdm);
  fread(fid,1,'int32');
  [i count]=fread(fid,1,'int32');
end
fclose(fid);

fid=fopen('fort.13','r','b');
[i count]=fread(fid,1,'int32');
n=0;
while count~=0
  n=n+1;
  eta(:,:,n)=reshape(fread(fid,idm*jdm,'float64'),idm,jdm);
  fread(fid,1,'int32');
  [i count]=fread(fid,1,'int32');
end
fclose(fid);

i=find(ub==eta(1,1,1));
ub(i)=nan;
i=find(vb==eta(1,1,1));
vb(i)=nan;
i=find(eta==eta(1,1,1));
eta(i)=nan;

pcolor(eta(:,:,end)'*0.081);shading flat;caxis([-.5 .5]);colorbar
