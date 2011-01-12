//usepackage("minijs");
real w=140;
real h=30;
real s=50;

real o=30;

draw((0,o)--(0,h+o)--(w,h+o)--(w,0+o)--(0,o)--cycle);
draw((s,o)--(s,h+o));
label("\textsf{Size}", (s/2,h/2+o));
label("\textsf{Body}", (s+20,h/2 - 1+o));
draw((s+10,20)--(s+10,o), Arrow);
label("\textsf{Pointer}", (s+15,10));
