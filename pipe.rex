/* PIPE
   Version 0.2
   2025/05/07
   (c) Michael Beer
*/

parse arg s
data.0=0
labels=""
label_mat.=0
vars. = ""

pipesep = "|"  /* pipe separator character */

/* load config */
config.=
file="PIPE.CFG"
ret=stream(file,"C","OPEN")
do while lines(file)
   line=linein(file)
   if left(line,1)="*" then iterate
   if line="" then iterate
   parse var line key rest
   config.key = rest
   if key=pipesep then pipesep=rest
end
ret=stream(file,"C","CLOSE")




if left(s,1)="*" then do
   parse var s "*" pipefile  pipedata
   if pos(".",pipefile)=0 then pipefile=pipefile".pipe"
   ret=stream(pipefile,"C","OPEN")
   s=""
   do while lines(pipefile)>0
      line=linein(pipefile)
      s=s||line"|"
      end
   ret=stream(pipefile,"C","CLOSE")

   if pipedata<>"" then do
      ret=stream(pipedata,"C","OPEN")
      index=0
      do while lines(pipedata)>0
         line=linein(pipedata)
         index+=1
         data.index = line
         data.0=index
      end
      ret=stream(pipedata,"C","CLOSE")
    end
end

if left(s,1)="?" then do
   s=""
   say "ENTER PIPE commands"
   do forever
      parse pull line
      if line="" then leave
      s=s||line"|"
      end
end

if upper(word(s,1))="PIPESEP" then do
   pipesep=word(s,2)
   s=delword(s,1,2)
end

s=changestr(" ",s,"$$$")
s=changestr("||",s,"$ESC$")   /* | is escaped as || */
s=changestr(pipesep,s,"|")
s=changestr("|",s," ")
s=s" *END"

max_stages = words(s)
if max_stages=0 then do
   say "nothing to do"
   exit
end
do i=1 to max_stages
   x=word(s,i)
   x = changestr("$$$",x," ")
   if pos(": ",x)>0 then do
      parse var x label ":" x
      label=upper(label)
      if wordpos(label,labels)>0 then do
         say "ERROR. Duplicate Label definition"
         exit
      end
      else do
         labels=labels label
         p=wordpos(label,labels)
         label_mat.p = i
      end
   end

   stage.i = strip(x)
end

newstage=0

do curr_stage=1 to max_stages
   if newstage<>0 then do
      curr_stage=newstage   /* change flow */
      newstage=0
      end
   line=stage.curr_stage
   line=changestr("$ESC$",line,"|")
   do while pos("$",line)>0
      parse var line part1 "$" var "." part2
      var=upper(var)
      select
         when var="LINES" then var=data.0
         when var="DATE"  then var=date("S")
         when var="ISODATE"  then var=date("I")
         when var="TIME"  then var=time("NS")
         otherwise var=vars.var
      end
      line=part1 var part2
   end
   ret=process(line)
end
return 0




process:

parse arg cmd
cmd=strip(cmd)
cmd_word=upper(word(cmd,1))

select
   when left(cmd,2)="--" then nop
   when left(cmd,1)="*" then nop
   when left(cmd,2)="<<" then do
      parse var cmd "<<" file
      file=strip(file)
      ret=stream(file,"C","OPEN")
      index=data.0
      do while lines(file)>0
         line=linein(file)
         index+=1
         data.index = line
         data.0=index
      end
      ret=stream(file,"C","CLOSE")
   end

   when cmd_word="EXIT" then exit

   when left(cmd,1)="<" then do
      parse var cmd "<" file "(" parms
      file=strip(file)
      if parms<>"" then parse var parms "SEP" sepchar .
      if datatype(sepchar)="NUM" then sepchar=d2c(sepchar)

      ret=stream(file,"C","OPEN")
      index=0
      do while lines(file)>0
         line=linein(file)
         if length(sepchar)>0 then do
/*
            line=changestr(" ",line,"$$$")
*/
            line=changestr(sepchar,line," ")
         end
         index+=1
         data.index = line
         data.0=index
      end
      ret=stream(file,"C","CLOSE")
   end

   when left(cmd,2)=">>" then do
      parse var cmd ">>" file
      file=strip(file)
      ret=stream(file,"C","OPEN WRITE APPEND")
      do i=1 to data.0
         ret=lineout(file,changestr("$$$",data.i," "))
      end
      ret=stream(file,"C","CLOSE")
   end

   when left(cmd,1)=">" then do
      parse var cmd ">" file
      file=strip(file)
      ret=stream(file,"C","OPEN WRITE REPLACE")
      do i=1 to data.0
         ret=lineout(file,changestr("$$$",data.i," "))
      end
      ret=stream(file,"C","CLOSE")
   end

   when cmd_word="CSV" then do
      parse var cmd . "SEP" sep
      sep=space(sep)
      if sep="" then sep=","
      call copy2work
      call clear

      do i=1 to work.0
         line=work.i
         do while countstr('"',line)//2 <>0
            i+=1
            line=line work.i
         end

         line = changestr('""',line,"$$$")

         do while pos('"',line)>0
            parse var line part1 '"' fld '"' part2
            fld=changestr(",",fld," ")
            fld='$$$'fld'$$$'
            line=part1 fld part2
         end

         line = changestr(',,',line,",$$$,")
         line = changestr(" ",line,"$$$")
         line = changestr(sep,line," ")
         ret=add(line)
      end
   end


   when curr_stage=1 & abbrev("CONSOLE",cmd_word,4) then do
      index=0
      say "INPUT DATA:"
      do forever
         parse pull input
         if input="" then leave
         index+=1
         data.index=input
         data.0 = index
      end
   end

   when abbrev("INPUT",cmd_word,3) then do
      index=0
      say "INPUT DATA:"
      do forever
         parse pull input
         if input="" then leave
         index+=1
         data.index=input
         data.0 = index
      end
   end

   when cmd_word="GROUP" then do
      parse var cmd . watch_flds "(" parms                         /* flds = list of fields to watch for group change */
      if wordpos("DET",upper(parms))>0 then details=1
         else details=0
      if wordpos("SPACE",upper(parms))>0 then space=1
         else space=0
      if wordpos("GRPSPACE",upper(parms))>0 then grp_space=1
         else grp_space=0
      sum_flds=""

      do while wordpos("SUM",upper(parms))>0 then do

         parse upper var parms . "SUM" f1 f2 rest
         sum.1.from=f1
         sum.1.into=f2
         sum_flds=sum_flds f1
         parms = rest
      end

      call copy2work
      call clear
      sum. = 0
      new_fld.=""
      old_fld.=""

      do lc=1 to work.0
         new_line=work.lc
         do f=1 to words(new_line)                              /* split new line into fields */
            new_fld.f=word(new_line,f)
         end
         new_fld.0=words(new_line)

         if lc=1 then do
            do f=1 to words(new_line)                              /* set up 1st record */
               old_fld.f=word(new_line,f)
            end
            old_fld.0=words(new_line)
         end

         s=""
         grp_change=0
         if lc>1 then do j=1 to words(watch_flds)                               /* detect group change */
            fld = word(watch_flds,j)
            if old_fld.fld <> new_fld.fld then grp_change=1
         end

         if grp_change=1 then do                              /* action for group change */
            s=""
            do f=1 to old_fld.0                               /* get fields to summarize */
               if wordpos(f,sum_flds)>0 then s=s sum.f
               else if wordpos(f,watch_flds)>0 then s=s old_fld.f
                  else s=s "."
            end
            ret=add(s)
            if grp_space then ret=add("")

            sum.=0
            grp_change=0

            do f=1 to words(new_line)                              /* set up 1st record */
               old_fld.f=word(new_line,f)
            end
            old_fld.0=words(new_line)

         end  /* if grp_change */
         if details then ret=add(new_line)

         do f=1 to words(sum_flds)                         /* get fields to summarize */
            w=word(sum_flds,f)
            sum.w += new_fld.w
         end

      end /* do lc*/

      s=""                                               /* FINAL SUM */
      do f=1 to old_fld.0                               /* get fields to summarize */
         if wordpos(f,sum_flds)>0 then s=s sum.f
            else if wordpos(f,watch_flds)>0 then s=s old_fld.f
               else s=s "."
      end
      ret=add(s)
      if grp_space then ret=add("")
   end




   when curr_stage<>1 & abbrev("CONSOLE",cmd_word,4) then do
      do i=1 to data.0
         say changestr("$$$",data.i," ")
      end
   end

   when cmd_word="GOTO" then do
      parms=upper(word(cmd,2))
      if datatype(parms)="NUM" then newstage=parms
      else do
         p=wordpos(parms,labels)
         if p>0 then newstage=label_mat.p
      end
   end

   when cmd_word="IFZERO" then do
      cmd=changestr(" goto ",cmd," GOTO ")
      parse var cmd . var "GOTO" label
      var=strip(var)
      if datatype(left(var,1))="CHAR" then do
         x=value(var)
         x=vars.x
         if x=0 then do
            label=upper(label)
            p=wordpos(label,labels)
            if p>0 then newstage=label_mat.p
         end
      end
   end

   when cmd_word="FORMAT" then do
      call copy2work
      call clear
      wlen.=0
      do i=1 to work.0
         line=work.i
         do j=1 to words(line)
            w=word(line,j)
            wlen.j = max(wlen.j,length(w))
         end
      end
      do i=1 to work.0
         line=work.i
         s=""
         do j=1 to words(line)
            w=word(line,j)
            w=left(w,wlen.j)
            s=s w
         end
         ret=add(strip(s))
      end
   end

   when cmd_word="MAX" then do
      n=word(cmd,2)
      max=-99999999
      lc=1
      call copy2work
      call clear
      do i=1 to work.0
         line=work.i
         w=word(line,n)
         if datatype(w)<>"NUM" then iterate
         if w>max then do
            lc=i
            max=w
         end
      end
      ret=add(work.lc)
   end

   when cmd_word="MIN" then do
      n=word(cmd,2)
      min=99999999
      lc=1
      call copy2work
      call clear
      do i=1 to work.0
         line=work.i
         w=word(line,n)
         if datatype(w)<>"NUM" then iterate
         if w<min then do
            lc=i
            min=w
         end
      end
      ret=add(work.lc)
   end

   when cmd_word="SQL" then do
      parse upper var cmd . op data
      select
         when op="CREATE" then do
            call copy2work
            index=1
            data.1="CREATE TABLE" data "("
            data.0=index
            do i=1 to work.0
               line=work.i
               if left(line,1)="*" then iterate
               if line="" then iterate
               if i<work.0 then line=line","
               index+=1
               data.index=line
               data.0=index
            end
            index+=1
            data.index=");"
            data.0=index
            index+=1
            data.index="COMMIT;"
            data.0=index

            end
         otherwise nop
      end
   end


   when cmd_word="IF" then do
      cmd=changestr(" goto ",cmd," GOTO ")
      parse var cmd . condition "GOTO" label
      condition = space(condition)
      ret = eval(condition)
/*
      x = "ret="condition
      interpret x
*/
      if ret<>0 then do
         label = upper(label)
         p=wordpos(label,labels)
         if p>0 then newstage=label_mat.p
      end
   end

   when cmd_word="CLEAR" then do
      index=0
      data.=
      data.0=0
   end

   when cmd_word="LET" then do  /* set control variable */
      parse var cmd . var "=" contents
      x = value(var)
      if datatype(left(var,1))="CHAR" then vars.x = contents
   end

   when cmd_word="SEQ" then do /* APL IOTA */
      parse var cmd . n
      call clear
      if datatype(n)="NUM" then do
         do i=1 to n
            ret=add(i)
         end
      end
   end

   when cmd_word="UNSPACE" then do
      parse var cmd . sep
      if sep="" then sep=" "
      do i=1 to data.0
         line=data.i
         line=changestr("$$$",line,sep)
         data.i=line
      end
   end

   when cmd_word="SET" then do /* set words in data */
      parse var cmd . s
      s=strip(s)
      cond=""
      if left(s,1)="[" then parse var s "[" cond "]" s
      cond_template = cond

      call copy2work
      call clear

      lc=0
      do i=1 to work.0
         lc+=1
         fld.= ""
         fld.0 = 0
         line=work.i
         cond = cond_template
         if cond<>"" then do                                   /* check condition for record */
            do while pos("&",cond)>0
               parse var cond part1 "&" id "." part2
               id=word(line,id)
               cond=part1||id||part2
            end
            ret="cond="cond
            cond=eval(cond)
         end

         if cond=0 then iterate

         do j=1 to words(s)    /* w2=... */
            w=word(s,j)
            parse var w index "=" contents
            contents = subst(contents)
/*
            do while pos("&",contents)>0
               parse var contents part1 "&" id "." part2
               if id=0 then id=line
                  else id=word(line,id)
               contents=part1||id||part2
            end
*/
            index=substr(index,2)
            if left(contents,1)="(" then do
            contents=eval(contents)
/*
               ret="contents="contents
               interpret ret
*/
               end
            fld.index = contents
            fld.0 = max(fld.0,index)
         end
         line = ""
         do x=1 to fld.0
            line=line fld.x
         end
        line = strip(line)
        ret=add(line)
      end
   end

   when cmd_word="INC" then do
      parse var cmd . var
      if datatype(left(var,1))="CHAR" then do
         x=value(var)
         vars.x = vars.x+1
      end
   end

   when cmd_word="DEC" then do
      parse var cmd . var
      if datatype(left(var,1))="CHAR" then do
         x=value(var)
         vars.x = vars.x-1
      end
   end


   when cmd_word="EXEC" then do
      call copy2work
      index=0
      data.0=index
      parse var cmd . pre "/" post
      do i=1 to work.0
         object=work.i
         s=pre object post
         address system s with output stem o.
         do j=1 to o.0
            index+=1
            data.index=o.j
            data.0=index
         end
      end
   end

   when cmd_word="STATE" then do
      do i=1 to data.0
         file=data.i
         if stream(file,"C","QUERY EXISTS")<>"" then state=0
            else state=1
         data.i = file state
      end
   end

   when cmd_word="PUSH" then do
      do i=1 to data.0
         queue data.i
      end
   end

   when cmd_word="PULL" then do
      index=data.0
      do while queued()>0
         parse pull line
         index +=1
         data.index = line
         data.0 = index
      end
   end
   when left(cmd_word,1)="." then do /*external command */
      do i=1 to data.0
         queue data.i
      end

      cmd=substr(cmd,2)
      s="call" cmd
      interpret s

      index=data.0
      do while queued()>0
         parse pull line
         index +=1
         data.index = line
         data.0 = index
      end
   end

   when cmd_word="MAIL" then do
      file=word(cmd,2)
      mail_cmd=config.mail
      do i=1 to data.0
         line=data.i
         if left(line,1)="*" then iterate
         if line="" then iterate
         user=word(line,1)
         s=mail_cmd
         s=changestr("&USER.",s,user)
         s=changestr("&FILE.",s,file)
         address system s with output stem o.
      end
   end

   when cmd_word="CURL" then do
      call copy2work
      curl_cmd=config.curl
      index=0
      data.0=0
      do i=1 to work.0
         line=work.i
         if left(line,1)="*" then iterate
         if line="" then iterate
         url=word(line,1)
         s=curl_cmd '"'url'"'
         address system s with output stem o.
         index+=1
         data.index=""
         index+=1
         data.index="* * * " url
         data.0=index
         do j=1 to o.0
            line=o.j
            index+= 1
            data.index=line
            data.0=index
         end
      end
   end

   when cmd_word="BASE64" then do
      parse upper var cmd . op object
      if abbrev("LINE",object,1) then object="LINE"
         else object="FILE"
      if abbrev("DECODE",op,1) then op="DECODE"
         else op="ENCODE"
      call copy2work
      index=0
      data.0=0
      if object="LINE" then do
         do i=1 to work.0
            line=work.i
            if op="ENCODE" then s=base64encode(line)
            if op="DECODE" then s=base64decode(line)
            index+=1
            data.index=s
            data.0=index
         end
      end
      else do /* FILE */
         line=""
         do i=1 to work.0
            line=line work.i
         end
         line=strip(line)
         if op="ENCODE" then s=base64encode(line)
         if op="DECODE" then s=base64decode(line)
         len=length(s)
         do i=1 to len by 64
            x=substr(s,i,64)
            index+=1
            data.index=x
            data.0=index
         end
      end
   end


   when cmd_word="PIPE" then do
      type=""
      parse var cmd . name "(" parms
      if abbrev("SPAWN",upper(parms),1) then type="SPAWN"

      file=time("T")".tmp"
      ret=stream(file,"C","OPEN WRITE REPLACE")
      do i=1 to data.0
         ret=lineout(file,data.i)
      end
      ret=stream(file,"C","CLOSE")
      s = "*"name file
      if type="" then ret=pipe(s)
      else address system '"PIPE.REX ' s'"'

      index=0
      data.0=0
      ret=stream(file,"C","OPEN READ")
      do while lines(file)>0
         line = linein(file)
         index+=1
         data.index=line
         data.0=index
      end
      ret=stream(file,"C","CLOSE")
      "ERASE" file
   end

   when abbrev("LITERAL",cmd_word,3) then do
      if pos("(",cmd)>0 then do
         parse var cmd . line "(" parms
         parms=upper(parms)
         select
            when datatype(parms)="NUM" then data.parms=line
            when parms="BEFORE" then do
               do i=data.0 to 1 by -1
                  j=i+1
                  data.j=data.i
               end
               data.1 = line
               data.0 = data.0 + 1
            end
            otherwise nop
         end   /* select */
      end /* if */
      else do
         index=data.0
         index+=1
         data.index = substr(cmd,length(cmd_word)+2)
         data.0=index
      end
   end

   when cmd_word="SAY" then do
      say substr(cmd,5)
   end

   when abbrev("DUPLICATE",cmd_word,3) then do
      parms=word(cmd,2)
      if datatype(parms)="NUM" then n=parms
         else n=0
      call copy2work
      index=0
      do i=1 to work.0
         line=work.i
         do j=1 to n
            index+=1
            data.index = line
            data.0=index
         end
      end
   end

   when cmd_word="SPAWN" then do
      pgm = delword(cmd,1,1)
      address system "START" pgm
   end

   when cmd_word="MQTT" then do
     subcmd = upper(word(cmd,2))
     server = word(cmd,3)
     topic  = word(cmd,4)
     size   = word(cmd,5)
     timeout= word(cmd,6)
     if size= "" then size=512
     if timeout="" then timeout=10

     if abbrev("SUBSCRIBE",subcmd,3) then do
        cmd="curl mqtt://"server"/"topic " --output - --max-filesize" size "--max-time" timeout
        address command cmd with output stem o.
        string=""
        do i=1 to o.0
           string=string||o.i
        end

        topic_start=topic"{"
        entries = countstr(topic_start,string)
        pos=1
        do i=1 to entries
           p=pos(topic_start,string,pos)
           pos=p+1
           string=overlay("  ",string,p-2)
        end
        string=strip(string)

        string=changestr('"',string,"$QUOTE$")
        string=changestr(" ",string,"$$$")
        string=changestr(topic_start,string," ")

        index = data.0
        do i=1 to words(string)
           w=word(string,i)
           w=changestr("$$$",w," ")
           w=strip(w)
           w="{"w
           w=changestr("$QUOTE$",w,'"')
           index+=1
           data.index = w
           data.0=index
        end
      end /* SUBSCRIBE */
   end


   when abbrev("CHANGE",cmd_word,2) then do
      x = strip(delword(cmd,1,1))
      delimiter = left(x,1)
      parse var x (delimiter) from (delimiter) to (delimiter)
      do i=1 to data.0
         data.i=changestr(from,data.i,to)
      end
   end

   when abbrev("CHOP",cmd_word,3) then do
      x = word(cmd,2)
      do i=1 to data.0
         data.i=left(data.i,x)
      end
   end

   when cmd_word="PAD" then do
      x = word(cmd,2)
      delimiter=word(cmd,3)
      do i=1 to data.0
         data.i=left(data.i,x,delimiter)
      end
   end

   when cmd_word="STRIP" then do
      do i=1 to data.0
         data.i=strip(data.i)
      end
   end

   when cmd_word="SPACE" then do
      do i=1 to data.0
         data.i=space(data.i)
      end
   end

   when cmd_word="NOBLANK" then do
      call copy2work
      index=0
      data.0=0
      do i=1 to work.0
         line = space(work.i)
         if line<>"" then do
            index+=1
            data.index=line
            data.0=index
         end
      end
   end

   when cmd_word="HEAD" then do
      data.0 = 20
   end

   when cmd_word="TAIL" then do
      call copy2work
      max = data.0
      index=0
      data.0=0
      do i=max(max-20,1) to max
         index+=1
         data.index=work.i
         data.0=index
      end
   end

   when cmd_word="QUEUE" then do
      do i=1 to data.0
         queue data.i
      end
   end

   when cmd_word="PULL" then do
      index=data.0
      do while queued()>0
         parse pull line
         index+=1
         data.index = line
         data.0 = index
      end
   end

   when cmd_word="SORT" then do
      parse var cmd . target "(" parms
      parms=upper(parms)
      if wordpos("DESC",cmd) then sort_order="DESC"
         else sort_order="ASC"
      if wordpos("NUM",cmd) then sort_type="NUM"
         else sort_type="ASC"

      /* prepare data */
      target=upper(target)
      work.0 = data.0

      select
         when pos("W",target)=1 then do i=1 to data.0
            line=data.i
            wpos = substr(word(target,1),2)
            delimiter = word(target,2)
            if delimiter<>"" then do
               line=changestr(" ",line,"$$$")
               line=changestr(delimiter,line," ")
               end
            w = word(line,wpos)
            if delimiter<>"" then w=changestr("$$$",w," ")
            work.i=w
            end
         when pos("-",target)> 0 then do i=1 to data.0
            line=data.i
            parse var target p "-" q
            w = substr(line,p,q-p+1)
            work.i=w
            end
         when datatype(target)="NUM" then do i=1 to data.0
            line=data.i
            p=word(target,1)
            work.i = substr(line,p,1)
            end
         otherwise call copy2work
      end

      N = work.0
      I = N % 2
      DO WHILE I \== 0
         DO J = I + 1 TO N
            K = J
            P = K - I                                   /*P: Previous Item*/
            ZED  = work.J
            ZED2 = data.J
            DO WHILE K >= I+1 & work.P > ZED
               work.K = work.P
               data.K = data.P
               K = K - I
               P = K - I
            END; /*WHILE K=I+1*/
            work.K = ZED
            data.K = ZED2
         END; /*J*/
         IF I == 2 THEN
            I = 1
         ELSE
            I=I * 5 % 11
      END; /*WHILE*/
   end

   when cmd_word="TAKE" then do
      call copy2work
      n=word(cmd,2)
      sign=sign(n)
      n=abs(n)
      n=min(n,data.0)
      lines=data.0

      data.0=0
      index=0
      if sign>0 then do; start=1          ; end=n     ;end
      if sign<0 then do; start=(1+lines-n); end=lines ;end
      if sign<>0 then do i=start to end
         index+=1
         data.index = work.i
         data.0 = index
         end
   end

   when cmd_word="TOLABEL" then do
      call copy2work
      label = strip(delword(cmd,1,1))
      lines=data.0
      data.0=0
      index=0
      do i=1 to work.0
         line=work.i
         if line=label then leave
         index+=1
         data.index=line
         data.0=index
      end
   end

   when cmd_word="FRLABEL" then do
      call copy2work
      label = strip(delword(cmd,1,1))

      data.0=0
      index=0

      found=0
      do i=1 to work.0
         line=work.i
         if line=label then do found=i;leave;end
      end

      if found>0 then do
         do i=found to work.0
            index+=1
            data.index = work.i
            data.0 = index
         end
      end
   end

   when abbrev("UNIQUE",cmd_word,4) then do
      call copy2work
      data.0=0
      index=0
      cache=""
      do i=1 to work.0
         line=work.i
         if line<>cache then do
            cache=line
            index+=1
            data.index=line
            data.0=index
         end
      end
   end

   when abbrev("SPECS",cmd_word,4) then do
      call copy2work
      parms = delword(cmd,1,1)
      parms_upper=upper(parms)
      do i=1 to work.0
         line=work.i
         data.i=""
         parms = changestr("*",parms,length(line))
         if pos("/",parms)>0 then do
            parse var parms part1 "/" x "/" part2
            var=changestr(" ",x,"$$$")
            parms=part1"/"var"/"part2
         end

         do j=1 to words(parms) by 2
            from=word(parms,j)
            to  =word(parms,j+1)
            if upper(to)="NEXT" then to=length(data.i)+1
            if upper(to)="NEXTWORD" then to=length(data.i)+2

            write_flag=1
            select
               when upper(from)="FS" | upper(from)="FIELDSEP" then do
                  delimiter = to
                  line=changestr(" ",line,"$$$")
                  line=changestr(delimiter,line," ")
                  write_flag=0
                  end

               when left(upper(from),1)="W" then do
                  w=substr(from,2)
                  s=word(line,w)
                  end
               when left(upper(from),1)="F" then do
                  w=substr(from,2)
                  s=word(line,w)
                  end
               when left(from,1)="/" then do
                  parse var from "/" s "/"
                  s=changestr("$$$",s," ")
                  end
               when upper(from)="RECNO" then do
                  s=i
                  end
               when pos("-",from)>0 then do
                  parse var from col1 "-" col2
                  len=col2-col1+1
                  s=substr(line,col1,len)
                  end
               otherwise do
                  col1=from
                  len=1
                  s=substr(line,col1,len)
                  end
            end
            if write_flag=1 then data.i = overlay(s,data.i,to)
         end j
      end i
   end

   when cmd_word="DROP" then do
      call copy2work
      n=word(cmd,2)
      sign=sign(n)
      n=abs(n)
      n=min(n,data.0)
      lines=data.0

      data.0=0
      index=0
      if sign>0 then do; start=n+1        ; end=lines ;end
      if sign<0 then do; start=1; end=(lines-n); end
      if sign<>0 then do i=start to end
         index+=1
         data.index = work.i
         data.0 = index
         end
   end

   when cmd_word="COUNT" then do
      parms=delword(cmd,1,1)
      parms=upper(parms)
      count_bytes=0
      count_words=0
      count_lines=data.0
      count_min=999
      count_max=0
      do i=1 to data.0
         line=data.i
         len = length(line)
         count_bytes+= len
         count_words+= words(line)
         count_min = min(count_min,len)
         count_max = max(count_max,len)
      end
      s=""
      do i=1 to words(parms)
         w=word(parms,i)
         if left(w,1)="B" then s=s count_bytes
         if left(w,1)="W" then s=s count_words
         if left(w,1)="L" then s=s count_lines
         if left(w,3)="MIN" then s=s count_min
         if left(w,3)="MAX" then s=s count_max
      end
      s=strip(s)
      index=1
      data.0 = index
      data.index = s
   end

   when abbrev("SUMMARY",cmd_word,2) then do
      parse var cmd . n
      if n="" then n=5

      text=""
      do i=1 to data.0
         text=text data.i
      end
      text=strip(text)
      text=summary(text,n)
      index=0
      data.0=index
      s=changestr(" ",text,"$$$")
      s=changestr(".",s," ")
      do i=1 to words(s)
         sentence=word(s,i)
         sentence=changestr("$$$",sentence," ")"."
         index+=1
         data.index=strip(sentence)
         data.0=index
      end
   end

   when cmd_word="HR" then do
      parse var cmd . n
      if n="" | datatype(n)<>"NUM" then n=80
      ret=add(copies("-",n))
   end


   when cmd_word="CALC"  then do
      parms=delword(cmd,1,1)
      parse var parms parms "(" clear_flag
      parms=upper(parms)
      calc_sum=0
      calc_avg=0
      calc_min=9999999
      calc_max=0
      do i=1 to data.0
         line=data.i
         x = word(line,1)
         if datatype(x)<>"NUM" then iterate
         calc_sum+= x
         calc_min = min(calc_min,x)
         calc_max = max(calc_max,x)
      end
      s=""
      do i=1 to words(parms)
         w=word(parms,i)
         if left(w,3)="SUM" then s=s calc_sum
         if left(w,3)="AVG" then s=s calc_sum/data.0
         if left(w,3)="MIN" then s=s calc_min
         if left(w,3)="MAX" then s=s calc_max
      end
      s=strip(s)
      if clear_flag<>"" then call clear
      ret=add(s)
   end


   when cmd_word="COMMAND" then do
     if words(cmd)>1 then s=delword(cmd,1,1)
     else s=data.1
     address command s with output stem data.
   end

   when cmd_word="SYSTEM" then do
     address system delword(cmd,1,1) with output stem data.
   end

   when cmd_word="SPLIT" then do
      call copy2work
      data.=
      index=0
      do i=1 to work.0
         line=work.i
         do j=1 to words(line)
            w=word(line,j)
            index+=1
            data.index = w
            data.0 = index
         end
      end
   end

   when cmd_word="JOIN" then do
      call copy2work
      data.=
      index=0
      line=""
      do i=1 to work.0
         line=line work.i
      end
      line=strip(line)
      index=1
      data.index=line
      data.0=index
   end

   when cmd_word="VAR" then do
      id=poolid()
      varname=delword(cmd,1,1)
      ret=value(varname,data.1,id-2)
   end

   when cmd_word="SAVE" then do
      pipe_name=delword(cmd,1,1)
      pipe_name="PIPE_"||strip(pipe_name)
      do i=1 to data.0
         ret=value(pipe_name||"."i,data.i)
      end
      ret=value(pipe_name||"."0,data.0)
   end

   when cmd_word="STEM" then do
      stem_name=delword(cmd,1,1)
      parse var stem_name stem_name "." .
      stem_name=strip(stem_name)
      do i=1 to data.0
         ret=value(stem_name||"."i,data.i,poolid()-2)
      end
      ret=value(stem_name||"."0,data.0,poolid()-2)
   end

   when cmd_word="LOAD" then do                           /* load stem into data */
      stem_name=delword(cmd,1,1)
      parse var stem_name stem_name "." .
      stem_name=strip(stem_name)
      if stem_name<>"" then do
         max=value(stem_name||"."0,,poolid()-2)

         data.=

         do i=1 to max
            data.i = value(stem_name||"."i,,poolid()-2)
         end
         data.0 = max
      end
      else do /* get from stack */
         index=data.0
         do while queued()>0
            parse pull line
            index +=1
            data.index = line
            data.0 = index
         end
      end  /* else do */
      end

   when cmd_word="GET" then do
      pipe_name=delword(cmd,1,1)
      pipe_name="PIPE_"||strip(pipe_name)
      max=value(pipe_name||"."0)
      data.0=max
      do i=1 to max
         data.i=value(pipe_name||"."i)
      end
   end


   when cmd_word="FINDIN" then do
      stem2=word(cmd,2)
      stem2 = "PIPE_"||strip(stem2)
      call copy2work

      data.=
      index=0
      PIPE_other.=
      other_index=0

      do i=1 to work.0
         line=work.i
         key=word(line,1)
         do j=1 to value(stem2||"."0)
            line2=value(stem2||"."j)
            key2=word(line2,1)
            if key=key2 then do; index+=1; data.index = line; data.0 = index; end
               else do; say key key2;other_index+=1; PIPE_other.other_index = line; PIPE_other.0 = other_index; end
         end j
      end i
   end

   when ABBREV("LOCATE",cmd_word,3) then do
     col1=0
     col2=0
     word=0
     if words(cmd)>=3 then do /*  LOCATE x-y /target/ */
        range=upper(word(cmd,2))
        if left(range,1)="W" then word=substr(range,2)
        if pos("-",range)>0 then parse var range col1 "-" col2
        target=delword(cmd,1,2)
     end
     else target=delword(cmd,1,1)
     delim=left(target,1)
     target=changestr(delim,target,"")

     call copy2work
     call clear
     call clear_other

     do i=1 to work.0
        line = work.i
        if word<>0 then line=word(line,word)
        if col2<>0 then line=left(line,col2)
        if col1<>0 then line=substr(line,col1)

        if pos(target,line)=0 then ret=add_other(line)
           else ret=add(line)
     end
/*
do i=1 to data.0; say data.i;end
do i=1 to pipe_other.0;say pipe_other.i;end
*/

   end

   when cmd_word="FIND" then do
     target=word(cmd,2)
     call copy2work
     data.=
     index=0
     PIPE_other.=
     other_index=0

     do i=1 to work.0
        if pos(target,work.i)=1 then do
           index+=1
           data.index = work.i
           data.0 = index
           end
         else do
           other_index+=1
           PIPE_other.index=work.i
           iterate
           end
     end /* do */
   end

   when cmd_word="NFIND" then do
     target=word(cmd,2)
     call copy2work
     data.=
     index=0
     PIPE_other.=
     other_index=0

     do i=1 to work.0
        if pos(target,work.i)=0 then do
           index+=1
           data.index = work.i
           data.0 = index
           end
         else do
           other_index+=1
           PIPE_other.index=work.i
           iterate
           end
     end /* do */
   end

   when abbrev("NLOCATE",cmd_word,4) then do
     target=word(cmd,2)
     delim=left(target,1)
     target=changestr(delim,target,"")
     call copy2work
     data.=
     index=0
     do i=1 to work.0
        if pos(target,work.i)<>0 then iterate
        index+=1
        data.index = work.i
        data.0 = index
     end
   end
/*
   when state(cmd_word".rex")=0 then do
      address system  cmd_word".rex" with input stem data. output stem data.
      end
*/

   when cmd_word="NUMBER" then do    /* number lines*/
      do i=1 to data.0
         line = i data.i
         data.i = line
      end
   end

   when cmd_word="SLEEP" then do    /* wait for n seconds */
      n=word(cmd,2)
      if n="" then n=5
      ret=sleep(n)
   end

   when cmd_word="XLATE" then do
     parms=delword(cmd,1,1)
     parms_upper = upper(parms)
     do i=1 to data.0
        select
           when parms_upper = "UPPER" then data.i=upper(data.i)
           when parms_upper = "LOWER" then data.i=lower(data.i)
           otherwise nop
        end
     end
   end

   when abbrev("REVERSE",cmd_word,3) then do
      call copy2work
      index=0

      do i=work.0 to 1 by -1
         line = work.i
         index+=1
         data.index = line
      end
   end

   when cmd_word="HELP" then do
      file="PIPE.HLP"
      ret=stream(file,"C","OPEN")
      index=0
      do while lines(file)>0
         line=linein(file)
         say line
         index+=1
         data.index = line
         data.0=index
      end
      ret=stream(file,"C","CLOSE")
      end

   otherwise do
      do i=1 to data.0    /* prepare data */
         queue data.i
      end

      s="call" cmd
      interpret s
      if rc<>"RC" then say "unknown command" cmd
      else do
         index=data.0
         do while queued()>0
            parse pull line
            index +=1
            data.index = line
            data.0 = index
         end
      end
      end
end
return 0

copy2work: procedure expose data. work.
work.=
do i=1 to data.0
   work.i=data.i
end
work.0=data.0
return
state: procedure
/* STATE
   check existence of a file
   input: filename
   output: 1 does NOT exist
           0 does exist
*/
parse arg file
ret=1
if stream(file,'C','QUERY EXISTS')<>"" then ret=0
return ret


base64decode: procedure
parse arg txt '='

BaseChars = xrange('A', 'Z')xrange('a', 'z')xrange('0', '9')'+/='
res = ''
val = translate(txt, xrange(, '3f'x), BaseChars)
do i = 1 to length(val)
   res = res||substr(x2b(c2x(substr(val, i, 1))), 3)
end
return x2c(b2x(left(res, length(res) %8 *8)))

base64encode: procedure
parse arg buf
s=xrange("A","Z")||xrange("a","z")||xrange("0","9")||"+/"
buflen=length(buf)
fill="="
ret=""

do i=1 by 3 to buflen
   c=substr(buf,i,3)
   c=strip(c)
   packetlen=length(c)
   c=c2x(c)
   c=x2b(c)  /* max 24 bits now */
   c=left(c||copies("0",24),24)
   x=""
   do j=1 by 6 to 19 /* take chunks of 6 bits */
      x=x||substr(s,x2d(b2x(substr(c,j,6)))+1,1)
      end
   if packetlen<3 then x=changestr("A",x,"=")
   ret=ret||x
end
return ret



summary: procedure
parse arg text,n
      maxsent = n /* number of sentences in output */

ret=""
      text = changestr("?",text,".")
      text = changestr("!",text,".")
      do while pos("..",text)>0
         text = changestr("..",text,".")
         end
    /* text is now in text */

/* GET SENTENCES */

     t=text
     i=0
     do while pos(".",t)>0
        parse var t a "." b
        i += 1
        s.i = a
        t = b
     end
    s.0 = i
    maxsent = min(s.0,maxsent)

   /* sentences are now in s. */

/* GET FREQUENCY FOR EACH WORD */
      k="" /* list of keywords found */
      f.=0
      fmax=0
      hiscore = 0

      wordtext=changestr(".",text," ")

      max = words(wordtext)
      do i=1 to max
         w=word(wordtext,i)
         p=wordpos(w,k)
         if p=0 then do
            k=k w
            fmax += 1
            f.fmax = 1
            end
         else do
            f.p += 1
            if f.p > hiscore then hiscore = f.p
            end
      end /* do i */

/* list of keywords is in k, size of k => maxwords */
      maxwords = words(k)

      total_words  = words(wordtext)

      do i=1 to maxwords
        f.i = f.i / total_words
        end


   used = ""

   do curr_s = 1 to maxsent
      max = 0 /* current scoring maximum */
      maxpos = 0 /* position of sentence s. */


   /* get sentence propability */

       do i=1 to s.0
         sentence = s.i
         n = words(sentence)

         if n=0 then iterate

         x = 0
         do j=1 to n
            w = word(sentence,j)
            p = wordpos(w,k)
            x = x + f.p
            end /* do */

         v.i = x/n
         if v.i > max & wordpos(i,used)=0 then do
            used = used i
            max = v.i
            maxpos = i
            end
         end /* if */


         sentence = s.maxpos
         ret=ret sentence"."

         do j=1 to words(sentence)
            w=word(sentence,j)
            p=pos(w,k)
            f.p = f.p ** 2
            end


      end /* do curr_s */

      ret=strip(ret)
return ret

/* add line to stage */
add: procedure expose data.
parse arg line
index = data.0
index += 1
data.index = line
data.0=index
return index


/* add line to other stage*/
add_other : procedure expose PIPE_other.
parse arg line

index = PIPE_other.0
index += 1
PIPE_other.index = line
PIPE_other.0 = index
return index


/* clear stage */
clear: procedure expose data.
data.=
data.0=0
return 0

/* clear other stage */
clear_other: procedure expose PIPE_other.
PIPE_other.=
PIPE_other.0=0
return 0

/* iff(cond,true-result,false-result) */
iff: procedure
parse arg cond,t,f
/*

s="ret=("||cond ")"

interpret s
if ret=1 then return t
 else return f
*/
return arg(3-arg(1))

eval: procedure
parse arg cond
ret="cond="cond
interpret ret
return cond


subst: procedure expose line lc
parse arg s

do while pos("&",s)>0
   parse var s part1 "&" id "." part2
   id = upper(id)
   select
      when id=0 then id=line
      when id="LRECL" then id=length(s)
      when id="LC" then id=lc
      when id="DATE" then id=date("S")
      otherwise if datatype(id)="NUM" then  id=word(line,id)
   end
   s = part1||id||part2
end
return s
