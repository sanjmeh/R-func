library(magrittr)
library(RMySQL)
library(DBI)
library(stringr)
library(formattable)
library(dplyr)
library(lubridate)
library(tibble)
library(png)
library(RCurl)
setwd("~/Dropbox/R-wd")
monitored_users<-496:505
database_name <- "jupiter_dev"
dbname<-database_name
#dbaccessfile<-"localdbaccess"
dbaccessfile<-"remotedbaccess"

load(dbaccessfile)

refresh<-function(variable_name=NULL,time_gap_hours=24){
    if(!exists(variable_name) || is.null(attr(get(variable_name),"last_upd")) || time_length(now()-attr(get(variable_name),"last_upd"),unit="hour") > time_gap_hours) {
    updated_df<-switch(variable_name,
               "st_tr100"= suppressWarnings(querysql("SELECT * FROM state_transitions where transition_time>DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
               "sessions100"=suppressWarnings(querysql("SELECT * FROM class_sessions where starts_on >DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
               "ss_time"=suppressWarnings(querysql("select * from stud_session_time where last_seen> DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
               "assesm"=suppressWarnings(querysql("select * from assessment_answers where last_updated> DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
              "quest_log"=suppressWarnings(querysql("select * from question_log where start_time> DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
              "topic_log"=suppressWarnings(querysql("select * from topic_log where transition_time> DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
              "ans_opt"=suppressWarnings(querysql("select * from answer_options where updated_on> DATE_SUB(NOW(),INTERVAL 100 DAY) ")),
              "dquestions"=suppressWarnings(querysql("SELECT * FROM questions" )),
              "dtopics"=suppressWarnings(querysql("SELECT * FROM topic" )),
              "dsubjects"=suppressWarnings(querysql("SELECT * FROM subjects" )),
              "dclasses"=suppressWarnings(querysql("SELECT * FROM classes" )),
              "dlesson_plan"=suppressWarnings(querysql("SELECT * FROM lesson_plan" )),
              "dtbl_auth"=suppressWarnings(querysql("SELECT * FROM tbl_auth" )),
              "st_index"=suppressWarnings(querysql("SELECT * FROM student_index" )),
              "si_map"=suppressWarnings(querysql("SELECT * FROM school_index_map" )),
               "Error")
    updated_df_withtime<-update_time(updated_df)
               } 
    if(exists("updated_df_withtime")) updated_df_withtime else get(variable_name)
}

update_time <- function(df){
    attr(df,"last_upd") <- now()
    return(df)
}


tables<-function(search=NULL,dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address,port=port)  # connect the DB at Amazon
    if(!is.null(search) && NROW(x<-grep(search,tables(),ignore.case = T,value=T))>0) return (x) else 
        if (is.null(search)) return(dbListTables(db)) else 
        return("No match found")
    on.exit(dbDisconnect(db))
}

tabs<-tables()

show_tbl_status<-function(dbname=database_name){
    querysql("show table status",database = dbname) %>% select("Name", "Rows", "Avg_row_length", "Data_length", "Index_length", "Auto_increment","Create_time","Update_time") %>% 
        mutate(updated=ymd_hms(Update_time),created=ymd_hms(Create_time),No_rows=format(Rows,big.mark=","),Auto_incr=format(Auto_increment,big.mark=","),Avg_row_lgth=format(Avg_row_length,big.mark = ","),Indx_length=format(Index_length,big.mark=","),Data_lgth=format(Data_length,big.mark=",")) %>% 
        select (-Create_time,-Update_time,-Index_length,-Avg_row_length,-Data_length,-Rows,-Auto_increment)
}

list_of_functions<-function(){
    lines<-scan(file="~/Dropbox/R-wd/quick.R",what=character(),skip = 28)
    #grepl("function",lines)
    i<-grepl("function",lines)
    fns_grouped<-{lines[i] %>% strsplit("<-")}
    listfns<-NULL
    for(i in 1: NROW(fns_grouped)){
        listfns<-c(listfns,fns_grouped[[i]][1])
    }
    sort(listfns)
}

display_image<-function(qid=NULL, baseurl='http://54.251.104.13/images/uploads_webapp/'){
    if(!is.null(qid)){
        plot(x = c(0,1200),y=c(0,800))
        pic_url<-paste0(baseurl,"q-",qid,".png")
        content<-getURLContent(pic_url)
        png_obj<-readPNG(content,info = T)
        rasterImage(png_obj,0,0,1024,768)
    }
    
}


display_ipad_image<-function(ass_ansid=NULL,image_path=NULL, baseurl='http://54.251.104.13/images/'){
    if(is.null(ass_ansid)){
        pic_url<-paste0(baseurl,image_path)
    } else 
    {
        image_path<-querysql(paste0("select image_path from uploaded_images where image_id=(select scribble_id from answer_options where assessment_answer_id=",ass_ansid,")"))
        pic_url<-paste0(baseurl,image_path)
    }   
    content<-getURLContent(pic_url)
    png_obj<-readPNG(content,info = T)
    plot(x = c(0,1200),y=c(0,800))
    rasterImage(png_obj,0,0,1024,768)    
}


classid<-function(session_id){
    script<-paste("select class_id from class_sessions where class_session_id=",session_id)
    querysql(script) [,1]
}

school<-function(school_id=NULL){
    if(!is.null(school_id) && is.numeric(school_id)) script<-paste("SELECT * from schools WHERE school_id=",school_id) else
        script<-paste("SELECT * from schools")
    querysql(script)
}

move_school<-function(user=NULL,school_id=NULL){
    old_school<-user_status(user)$school_id
    if (NROW(old_school)==0) return("USER IS INVALID")
    if(nrow(school(school_id))==0) return("SCHOOL IS INVALID") else if(NROW(old_school)==1) {
        script<-paste("UPDATE tbl_auth SET school_id= ?x WHERE user_id=?y LIMIT 1")
        SQL<-sqlInterpolate(ANSI(),script,x=school_id,y=user)
    }  else if(NROW(old_school)>1) return("The script to move more than one student at a time is still not ready")
        # if (identical(old_school,rep(school_id,NROW(old_school)))) return("ALL USERS ALREADY IN SAME SCHOOL")  else
        # {
        #     #we have problem in this code, as noquote not working..to post on StackOverflow
        #     # user_string<-paste(user,collapse=",")
        #     # script<-paste("UPDATE tbl_auth SET school_id= ?x WHERE user_id in (?y) LIMIT ?z")
        #     # SQL<-sqlInterpolate(ANSI(),script,x=school_id,y=user_string,z=NROW(old_school))
        #
        # }

    runsql(SQL)
}

school_mismatches<-function(students=F,detail=F){
    mstudents=NULL
    mteachers<-ta("schools") %>% 
        merge(ta("acad")) %>% 
        merge(ta("classes")) %>% 
        merge(ta("tbl"),by.x="teacher_id",by.y="user_id") %>% 
        select(starts_with("class"),starts_with("sch"), starts_with("acad"), 
               starts_with("user_n"),starts_with("teacher"),starts_with("sub")) %>% 
        filter(school_id.x!=school_id.y) %>% 
        arrange(class_id)
    if(students) {
        SQL1<-paste("select student_id,class_id from student_class_map order by student_id")
        x<-querysql(SQL1) %>% merge(ta("tbl_"),by.x="student_id",by.y="user_id")

        y<-ta("schools") %>% 
            merge(ta("acad")) %>% 
            merge(ta("classes"))
            
        mstudents<-left_join(x,y,by="class_id") %>%  select(starts_with("stude"),user=user_name, school_id.x, 
                                                 school_id.y, starts_with("class"),starts_with("acad")) %>% 
            filter(school_id.x!=school_id.y)
    }
    mismatched<-list(Teachers=mteachers,Students=mstudents)
if(!detail) mismatched<-list(Mismatched_Teacher_classes=mteachers$class_id,Mismatched_teachers=mteachers$teacher_id,Mismatched_students=unique(mstudents$user),Mismatched_student_classes=mstudents$class_id)
    mismatched
}





# trans_mat<-function(x=st_tr100){
#     x %>% 
#         filter(entity_type_id==2) %>% tidyr::spread(key = from_state,to_state) ->tm
#     colnames(tm)[c(5,6,8)]<-c("Termination","Beginning","Opening")
#     tm$transition_time %<>% ymd_hms()
#     tm$`3`<-NULL
#     tm$`5`<-NULL
#     tm$`6`<-NULL
#     tm %>% arrange(transition_time)
# }

# st_tr100$transition_time %<>% ymd_hms()
# find_students<-function(session_id=NULL){
#     transition_matrix<-trans_mat(st_tr100)
#     time1<-{transition_matrix %>% filter(entity_id==session_id,!is.na(Beginning)) %>% select(transition_time)}$transition_time
#     time2<-{transition_matrix %>% filter(entity_id==session_id,!is.na(Termination)) %>% select(transition_time)}$transition_time
#     student_list<-stud(session = session_id)
#     st_tr100 %>% filter(between(transition_time,time1,time2),entity_type_id==1) %>% 
#         left_join(student_list,by=c("entity_id"="student_id")) %>% 
#         filter(!is.na(first_name)) %>% 
#         distinct(entity_id)
# }

stud_mismatches<-function(){
    assesm %>% group_by(class_session_id) %>% count(studcnt_answers=n_distinct(student_id)) ->assessm_attend
    ss_time %>% group_by(class_session_id) %>% summarise(present=sum(present),count_sstime=n())->sstime_attendance
    left_join(sessions100,sstime_attendance) %>% left_join(assessm_attend)->attendance_merged
    attendance_merged %>% select(class_session_id,class_id,teacher_id,total_stud_registered,stud_attended,present,count_sstime,studcnt_answers,session_state,session_time,starts_on,ends_on,n)
}


insert_class<-function(class_name=NULL,grade=NULL,section=NULL,teacher_id=NULL,acad_term=NULL,subject=NULL){
    script<-"INSERT INTO classes (class_name,grade_id,section_id,subject_id,academic_term_id,teacher_id) VALUES (?name, ?grade, ?sec, ?sub, ?acad, ?t )"
    script<-sqlInterpolate(ANSI(),script,name=class_name,grade=grade,sec=section,sub=subject,acad=acad_term,t=teacher_id)
    runsql(script)
}

insert_subj<-function(school_id=NULL,subject_name=NULL){
    script<-"INSERT INTO subjects (school_id,subject_name) VALUES (?sc, ?sub)"
    script<-sqlInterpolate(ANSI(),script,sc=school_id,sub=subject_name)
    runsql(script)
}

insert_acad_term<-function(school_id=NULL,text=NULL,startdate=date(now()),enddate=date(now()+ddays(90))){
    script<-"INSERT INTO academic_terms (school_id,description,start_date,end_date) VALUES (?sc, ?de, ?sd, ?ed )"
    script<-sqlInterpolate(ANSI(),script,sc=school_id,de=text,sd=as.character(startdate),ed=as.character(enddate))
    runsql(script)
    querysql(paste("SELECT * FROM academic_terms where school_id=",school_id))
}

delete_acad_term<-function(acad_id=NULL){
    script<-paste("DELETE FROM academic_terms WHERE academic_term_id=",acad_id, "LIMIT 1")
    runsql(script)
}

change_subject<-function(subj=NULL,class=NULL){
    if(!is.null(subj) && !is.null(class)) script<-"UPDATE classes SET subject_id = ?sub WHERE class_id= ?cl LIMIT 1" else return("error")
    script<-sqlInterpolate(ANSI(),script,sub=subj,cl=class)
    runsql(script)
}

change_teacher<-function(class=NULL,teacher=NULL){
    if(!is.null(class) && !is.null(teacher)) script<-"UPDATE classes SET teacher_id = ?t WHERE class_id= ?cl LIMIT 1" else return("error")
    script<-sqlInterpolate(ANSI(),script,cl=class,t=teacher)
    runsql(script)
}

change_class_name<-function(classid=NULL,new_name=NULL){
    if(!is.null(classid) && !is.null(new_name)) script<-sprintf("UPDATE classes SET class_name = '%s' WHERE class_id= %d LIMIT 1",new_name,classid) else return("error")
    runsql(script)
}

map_academic_term<-function(acad_term=NULL, class=NULL,school=NULL){
    if(is.null(acad_term)) return("Academic Term id is mandatory")
    
    if(is.null(class) && !is.null(school)) {
        at<-ta("acad") %>% filter(school_id==school)
        if(acad_term %in% at$academic_term_id) return("This is already mapped. No changes affected in DB") else
           { script<-"UPDATE academic_terms SET school_id = ?sch WHERE academic_term_id = ?a LIMIT 1"
             script<-sqlInterpolate(ANSI(),script,sch=school,a=acad_term)
           }
    } 
    if(!is.null(class) && is.null(school)) {
        classes<-ta("classes") %>% filter(academic_term_id==acad_term)
        if (class %in% classes$class_id) return("This class is already mapped to the correct school. No changes affected in DB") else 
        {
            script<-"UPDATE classes SET academic_term_id = ?a WHERE class_id= ?cl LIMIT 1"
            script<-sqlInterpolate(ANSI(),script,a=acad_term,cl=class)
        }
    }
    if(!is.null(class) && !is.null(school)) return("One extra argument. Expecting 2 and got 3")
    
    runsql(script)
}


class_list<-function(stud=NULL){
    scm<-ta("student_class_map")
    x<-scm %>% filter(student_id %in% stud) %>% group_by(student_id) %>% summarise(total_classes=n())
    y<-scm %>% filter(student_id %in% stud) %>% select(student_id,class_id)
}


valid_user<-function(user=NULL) if(nrow(user_status(user))==1) T else F
valid_teacher<-function(user=NULL) if(nrow(user_status(user) %>% filter(role_id==3))==1) T else F
valid_school<-function(sc=NULL) if(nrow(ta("schools") %>% filter(school_id %in% sc))==1) T else F

recent_topic_logs<-function(limit=20){
table_all("topic_log") %>% arrange(desc(topic_id)) %>% head(limit)
}




answers<-function(limit=20){
    suppressWarnings(querysql(paste("SELECT * FROM question_log ORDER BY question_log_id DESC LIMIT", limit))) ->qlog
       suppressWarnings(querysql(paste("SELECT * FROM assessment_answers ORDER BY assessment_answer_id DESC LIMIT",limit*10))) ->ans_log
       suppressWarnings(querysql(paste("SELECT * FROM answer_options ORDER BY assessment_answer_id DESC LIMIT",limit*40))) ->ans_option_log
       suppressWarnings(querysql(paste("SELECT * FROM student_index ORDER BY class_session_id DESC LIMIT",limit*5))) ->index


       ans_option_log %>%
         merge(ans_log) %>%
         group_by(assessment_answer_id,question_log_id,student_id) %>%
         summarise(options=n()) %>%
         merge(qlog) %>%
         merge(table_all("tbl_auth"),by.x = "student_id",by.y="user_id") %>%
         select(student_id:start_time,answer_count:first_name) %>%
         merge(dquestions) %>%
         select(-last_updated,-classification) %>%
         merge(dtopics) %>% head(20)
         # select(-(topic_info:pi)) %>%
         # merge (subjects) %>%
         # merge(classes) %>%
         # select(1:12)

           #select(1,"first_name",2:7) %>%
        # merge(table_all("class_sessions")) %>%
        # mutate(period_minutes=format(time_length(interval(starts_on,ends_on),unit="minute"),digits = 0)) %>%
        # select(-(ends_on:pi),-room_id) %>%
        # merge(table_all("classes")) %>%
        # rename(class="class_name") %>% select(4:18)
}

start_classes<-function(class=14,t=496,starts=0.2,size=1,number=3,gaps=0.25,room=20){
    runsql(gsinsert_class_sessions(class_id=class,teacher_id = t, gaps = gaps,number_rows = number,start_after = starts,duration = size,room_id = room))
}

add_student<-function(class_id=NULL,students=NULL){
    for (i in students){
        script<-"INSERT INTO student_class_map (student_id,class_id,last_updated) VALUES (?id1, ?id2, ?id3 ) "
        script_new<-sqlInterpolate(ANSI(),script,id1=i,id2=class_id,id3=as.character(now()))
        runsql(script_new)
    }
}

remove_student<-function(class_id=NULL,students=NULL){
  for (i in students){
    script<-"DELETE FROM student_class_map WHERE class_id=?class AND student_id=?stud"
    script_new<-sqlInterpolate(ANSI(),script,class=class_id,stud=i)
    runsql(script_new)
  }
}

common_students<-function(class1=NULL,class2=NULL){
    ids<-intersect(students(class1)$student_id,students(class2)$student_id)
    user_status(ids)
}


user<-function(name="Edna"){
table_all("tbl_auth") %>% filter(grepl(name,ignore.case = T, user_name))
}

rooms<-function(){
  grid_seat_counts() [,c(2,7)] %>% arrange(room_id)
}

change_room<-function(greater_than=NULL,room_id=NULL,limit=10){
    script<-"UPDATE class_sessions SET room_id=?x where class_session_id>?y limit ?li"
    script<-sqlInterpolate(ANSI(),script,x=room_id,y=greater_than,li=limit)
    runsql(script)
}

seating<-function(){
  sgrids<-grid_seat_counts()
    table_all("seat_assignments") %>%
        group_by(class_session_id,seat_state) %>%
        summarise(seats=n()) %>%
        merge(table_all("class_sessions"),by="class_session_id") %>%
        merge(sgrids,by="room_id") %>%
        select(-ends_with("on"),-ends_with("ed"))
}

seat_info<-function(session=NULL){
    #below script is used in PHP
    #script<- "select seat.seat_id, assign.seat_state from seats as seat inner join class_sessions as session on session.room_id = seat.room_id left join seat_assignments as assign
    #on seat.seat_id = assign.seat_id and assign.class_session_id = ?x left join entity_states as state on session.session_state = state.state_id where session.class_session_id = ?x";
    
    #new script
    script<-"select seat_id, seat_state from seat_assignments where class_session_id=?x"
    script<-sqlInterpolate(ANSI(),script,x=session)
    querysql(script)
}


gsinsert_class_sessions<-function(number_rows=1,start_after=1,
                                   duration=0.5, gaps = 0.5,class_id=14,room_id=25,teacher_id=496){

    query_text<-"INSERT INTO class_sessions (class_id,room_id,teacher_id,starts_on,ends_on,session_state)"
    value_text1<- paste0(" VALUES(",class_id, ",", room_id, ",", teacher_id, ", '", starting_time<-now() + dhours(start_after),"', '", ending_time<-starting_time+dhours(duration),"',4)")
    value_text_rest <-NULL
    for(i in 1:number_rows){
        value_text_rest<-paste0(value_text_rest, ", (", class_id, ",", room_id, ",",teacher_id,", '", starting_time<-ending_time + dhours(gaps), "', '", ending_time<-starting_time +dhours(duration),"',4)")
    }
    paste0(query_text,value_text1,value_text_rest)
}


shift<-function(session=NULL,min=5,what="both"){
  x<-show_sessions() %>% filter(session_state<5 & class_session_id==session)
  if(nrow(x)==1){
    new_end_time<-switch(what,both=x$ends_on + dminutes(min),end=x$ends_on + dminutes(min),x$ends_on)
    new_start_time<-switch(what,both=x$starts_on + dminutes(min),start=x$starts_on + dminutes(min),x$starts_on)
    script<-"UPDATE class_sessions SET starts_on=?nst, ends_on= ?net WHERE class_session_id= ?id LIMIT 1"
    new_script<-sqlInterpolate(ANSI(),script,nst=as.character(new_start_time),net=as.character(new_end_time),id=session)
    runsql(new_script)
  }else return("This session is either missing today or not in 4,2 or 1 state")
}


auto_cancel<-function(days=1){
  srows<-show_sessions(days = days) %>% filter(session_state<5 & ends_on <now())
  stud_dflist<-lapply(X = srows$class_id,FUN = students) 
  all_students<-Reduce(bind_rows,stud_dflist) %>% filter(user_state %in% c(1,9,10))
  s1<-sprintf("update class_sessions set session_state=5 where class_session_id=%d ",srows$class_session_id)
  s2<-sprintf("delete from live_session_status where session_id = %d",srows$class_session_id)
  s3<-sprintf("update tbl_auth set  user_state = 7 where user_id = %d",all_students$student_id)
  browser()
  runsql(s1);runsql(s2);runsql(s3)
}

replace_null_date<-function(table_name=NULL,date_name=NULL,key_name=NULL){
  # this is not yet working
  dump<-querysql(sprintf("select %s,%s from %s",key_name,date_name,table_name))
  browser()
  na_rows<-dump %>% filter(is.na(eval(date_name)))
  if (nrow(na_rows)>0) 
    na_rows %>% count(eval(key_name)) %>% 
    left_join(dump,by=eval(key_name)) %>% 
    filter(complete.cases(eval(key_name)))
}



transitions<-function(type=2,entityid=4054,limit=50){
    state_transitions(numbers = 5000) %>% filter(entity_type_id==type,entity_id==entityid) %>% head(limit)
}



show_sessions2<-function(number=10){
show_sessions(number=number) %>% merge(table_all("classes"),by = "class_id") %>% select(1:8,14,17,20) %>% merge(table_all("subjects"))
}

students<-function(class_id=NULL,session=NULL){
    if(!is.null(class_id)) querysql(paste('SELECT student_id FROM student_class_map WHERE class_id=',class_id)) %>% 
        merge(dtbl_auth,by.x="student_id",by.y="user_id") %>% 
        select(student_id,first_name,last_name,grade_id,user_state,school_id) else
    if(!is.null(session)) {
        script<- "select map.student_id from student_class_map as map inner join class_sessions as session on map.class_id = session.class_id where session.class_session_id = ?x"
        script<-sqlInterpolate(ANSI(),script,x=session)
        querysql(script) %>% 
            merge(dtbl_auth,by.x="student_id",by.y="user_id") %>% 
            select(student_id,first_name,last_name,grade_id,user_state,school_id)
    } else "Incorrect input"
}

stud_record<-function(){
    ta("assessment_answers") %>%
        mutate(total = n_distinct(ass)) %>%
        count(question_log_id, total) %>%
        mutate(proportion = percent(n / total)) %>% 
        arrange(desc(n)) %>% 
        select(-total,-n) %>%
        head(100)
    
}

stud_regist<-function(class="ALL"){
  all_classes<-querysql(paste('SELECT class_id,count(student_id) FROM student_class_map GROUP BY class_id'))
  if (length(class)==1 && grepl("A",class,ignore.case = T)) all_classes else
      all_classes %>% filter(class_id %in% class)
}

live_status<-function(){
  lv<-list(topic=NULL,sessions=NULL,users=NULL)
  x<-ta("live")
  y<-ss() %>% filter(session_state==1)
  z<-list_users() %>% filter(user_state==1)
  if(nrow(x)>0) x-> lv[[1]]
  if(nrow(y)>0) y %>% select(1:6) ->lv[[2]]
  if(nrow(z)>0) z %>% select(1,5,7:11) ->lv[[3]]
  lv
}

remove_ended<-function(){
    lss<-ta("live_session_status")
    sessions_to_check<-paste(lss$session_id,collapse = ",")
    if(sessions_to_check!='') extracted<-querysql(sprintf("SELECT class_session_id, ends_on FROM class_sessions WHERE class_session_id in (%s)",sessions_to_check)) else
        extracted<-NULL
    if(!is.null(extracted)) {
        to_be_deleted_sessions<-
    {extracted %>% filter(ends_on<now()) %>% select(class_session_id)}$class_session_id
    to_be_deleted_sessions_comma<-paste(to_be_deleted_sessions,collapse = ",")
    runsql(sprintf("DELETE FROM live_session_status WHERE session_id in (%s)",to_be_deleted_sessions_comma))
    } else 0
}

state_flash<-function(user=NULL){
    users<-list_users()
    stud_counts<-ta("student_class_map") %>%
        merge(users,by.x="student_id",by.y="user_id") %>% 
        select(class_id,student_id,user_state) %>% group_by(class_id,user_state) %>% summarise(stud_count=n())
    today_sessions<-show_sessions(u=user,days = 1,limit = 50)
    valid_sessions<-today_sessions$class_session_id
    seat_summary<-ta("seat_assignments") %>% group_by(class_session_id) %>% summarise(seats_configured=n()) %>% filter(class_session_id %in% valid_sessions)
    today_sessions <- inner_join(today_sessions,seat_summary,by="class_session_id")
    master<-merge(today_sessions,stud_counts,by="class_id")
   
    if(nrow(master)>0){
        master %<>% spread(key = user_state,stud_count,sep = ":")
        for (i in c(1,7,8,9,10,11)){
            if(length(intersect(names(master),var_col<-paste0("user_state:",i)))==0) master %<>% mutate(!!var_col := 0)
        }
        master%<>% inner_join(classes(u=user),by="class_id") 
        master %>% mutate(PreAllocatedSeats=`user_state:9`,OccupiedSeats=`user_state:1`+`user_state:10`,SessionGI=percent(gi_num.x/gi_den.x),SessionPI=pi.x,ClassGI=percent(gi_num.y/gi_den.y),ClassPI=pi.y,teacher_d=teacher_id.x) %>% 
            select(-stud_attended,-total_stud_registered,-starts_with("gi",ignore.case=F),-starts_with("pi",ignore.case=F),-teacher_id.y,-GI)
    } else "No class sessions for the day"
    
   
    #     select(SessionId=class_session_id, TeacherName=user_name,SubjectName=subject_name, SubjectId=subject_id, ClassName=class_name,StartTime=starts_on,EndTime=ends_on,SessionState=session_state,
    #            RoomName=room_name, SeatsConfigured=seat_counts,
    #             StudentsRegistered=tot_regist, PreAllocatedSeats=tot_alloc,OccupiedSeats=tot_joined,Grade=grade_id.x,TimeZone=timez)
    
}

role<-function(user=NULL) {list_users() %>% filter(user_id==user) %>% .[,"role_id"]}

classes<-function(user=NULL,school="ALL",detail=T){
    r<-data.frame(role_id=0)
    users<-ta("tbl_auth")
    subjects<-ta("subjects")
    #extract role
    if(!is.null(user) && valid_user(user)) r<- users %>% filter(user_id==user) %>% select(role_id)
    if (r$role_id==3){
        class_list<-
            classes() %>%
            filter(teacher_id==user) %>%
            merge(subjects) %>%
            select(starts_with("class"),school_id,teacher_id, TeacherName, grade_id,section_id, stud_regist,starts_with("subj"),gi_num,gi_den,pi) %>%
            mutate(GI=percent(gi_num/gi_den))
        if(!detail) class_list<-class_list$class_id
    } else
    if (r$role_id==4)
    {
        SQL1<-paste("select distinct(class_id) as class_id from student_class_map where student_id =", user)
        suppressWarnings(querysql(SQL1)) ->class_list_student
        class_list<-class_list_student$class_id
        if(detail) class_list<-
            classes() %>%
            filter(class_id %in% class_list) %>%
            merge(subjects) %>%
            select(starts_with("class"),school_id,teacher_id, TeacherName,grade_id,section_id, stud_regist,starts_with("subj"),gi_num,gi_den,pi) %>%
            mutate(GI=percent(gi_num/gi_den))
    } else 
        if(valid_school(school)) {
            acad_terms<-ta("acad") %>%   filter(school_id==school)
            class_list<- classes() %>% filter(academic_term_id %in% acad_terms$academic_term_id) %>% 
                merge(subjects) %>%
                select(starts_with("class"),school_id,teacher_id, TeacherName, grade_id,section_id, stud_regist,starts_with("subj"),gi_num,gi_den,pi) %>%
                mutate(GI=percent(gi_num/gi_den))
        } else 
            {
                class_list<- ta("classes") %>%  merge(subjects) %>% inner_join(users[c("user_id","user_name")],by=c("teacher_id"="user_id")) %>% 
                select(starts_with("class"),school_id,teacher_id, TeacherName=user_name, grade_id,section_id, stud_regist,starts_with("subj"),gi_num,gi_den,pi) %>%
                mutate(GI=percent(gi_num/gi_den))
            if(!detail) class_list<-class_list$class_id
            }
        class_list
}

valid_class<-function(class){
  if(NROW(class)==1 && nrow(classes() %>% filter(class_id==class))==1) T else F
}

cl<-function(pattern="NULL"){
  classes() %>% filter(grepl(pattern,class_name))
}

assign_class2teacher<-function(class=NULL,teacher=NULL){
  if(all(!is.null(teacher),!is.null(class))){
    if(valid_teacher(teacher)&& valid_class(class)) {
    script<-"UPDATE classes SET teacher_id=?x WHERE class_id=?y LIMIT 1"
    SQL<-sqlInterpolate(ANSI(),script,x=teacher,y=class)
    runsql(SQL)
    } else message("Either teacher is invalid or class is invalid")
  } else message("Either teacher is missed or class : Both class & teacher are mandatory inputs")
}

assign_class2student<-function(class=NULL,students=NULL){
  if(all(!is.null(students),!is.null(class))){
    x<-paste0("(",students);   y<-paste0(class,")");   value_list<-paste0(x,",",y,collapse=",")
    script<-"INSERT INTO student_class_map (student_id,class_id) VALUES ?x ON DUPLICATE KEY UPDATE class_id= ?y "
    SQL<-sqlInterpolate(ANSI(),script,x=value_list,y=class)
    #runsql(SQL)
    SQL
  } else message("Both student and class parameters are mandatory")
}

class_conflict<-function(classes=1:20){
  for (i in classes){
  }
}

grid_seat_counts<-function(){
    grids<-table_all("seating_grids")
    rcount<-grids$seats_removed %>% str_count(pattern=",") # count commas in the seats_removed column
    rcount<-ifelse(rcount,rcount+1,0) #add one to comma count
    grids$seat_counts<-grids$seat_rows*grids$seat_columns-rcount # subtract seats removed
    grids
}


list_XMPP_users<-function() {
    script<-"SELECT * FROM users"
    x<-suppressWarnings( querysql(script,database="ejabberd_1609"))
    x[,1:2]
}



questions<-function(pattern=NULL,topic=NULL,question=NULL,subj=NULL,.subj=NULL,width=30){
    dclasses<-refresh("dclasses")
    dquestions<-refresh("dquestions")
    dtopics<-refresh("dtopics")
    dsubjects<-refresh("dsubjects")
    dlesson_plan<-refresh("dlesson_plan")
    if(!is.null(subj)) {
        qids<-dquestions %>% left_join(dtopics,by="topic_id") %>% filter(subject_id==subj) %>% select(question_id) %>% .[,1]
        subj_name<-dsubjects %>% filter(subject_id==subj) %>% select(subject_name) %>% .[,1]
        return(questions(question = qids,.subj = subj_name))
    }
    tt<-c(is.null(pattern),is.null(topic),is.null(question))
    if(identical(tt,c(T,T,F))) filtered_list<-dquestions %>% filter(question_id %in% question)
    if(identical(tt,c(T,F,T))) filtered_list<-dquestions %>% filter(topic_id %in% topic)
    if(identical(tt,c(F,F,T))) filtered_list<-dquestions %>% filter(topic_id %in% topic,grepl(pattern,question_name,ignore.case = T))
    if(identical(tt,c(F,T,T))) filtered_list<-dquestions %>% filter(grepl(pattern,question_name,ignore.case = T))
    
    if(exists("filtered_list")) {
        count_classes<-filtered_list %>% left_join(dlesson_plan,by="topic_id") %>% left_join(dclasses[,1:7],by="class_id") %>% 
        group_by(question_id) %>% 
            summarise(no_classes=n(),class_list=paste(class_id,collapse = ","),teachers=paste(unique(teacher_id.y),collapse=","),class_sub=paste(unique(subject_id),collapse=","))
        filtered_list<-count_classes %>% left_join(filtered_list,by="question_id") 
    } 
    if (exists("filtered_list")){ 
        detailed_list<- filtered_list %>%  left_join(dtopics[,c("topic_id","topic_name","parent_topic_id")],by="topic_id") %>% 
            left_join(dtopics[,c("topic_id","topic_name")],by=c("parent_topic_id"="topic_id")) %>% 
            mutate(question_text=str_trunc(question_name,w=width,side = "right")) %>% 
            select(question_id,question_text,parent_topic_id,parent_topic_name=topic_name.y, topic_id,subtopic_name=topic_name.x,question_type_id,class_list,teachers,class_sub) 
        if(nrow(detailed_list)>0) detailed_list$subject_name=.subj else return (message("Warning: there are no questions to display in this subject?"))
        detailed_list
    } else return (message("Error: please check parameter sequence; are you giving too many or too few parameters?"))
}

replace_question<-function(question_id=NULL,text=NULL){
  script<-"UPDATE questions SET question_name=?y WHERE question_id=?x LIMIT 1"
  SQL<-sqlInterpolate(ANSI(),script,x=question_id,y=text)
  runsql(SQL)
}

replace_question_option<-function(option_id=NULL,text=NULL){
  script<-"UPDATE question_options SET question_option=?y WHERE question_option_id=?x LIMIT 1"
  SQL<-sqlInterpolate(ANSI(),script,x=option_id,y=text)
  SQL
}

replace_topic<-function(topic_id=stop("Topic_id cannot be null"),text=stop("Text cannot be null")){
  script<-"UPDATE topic SET topic_name=?y WHERE topic_id=?x LIMIT 1"
  SQL<-sqlInterpolate(ANSI(),script,x=topic_id,y=text)
  runsql(SQL)
}


display_question<-function(question=NULL){
    refresh("dquestions")->dquestions
    {dquestions %>% filter(question_id==question)}[,c(2,6)] ->question_title
    {dquestions %>% filter(question_id==question)}[,3] ->question_options
    print(question_title)
    print("-----------------")
    print(question_options)
}

dq<-display_question

pic<-function(path=NULL){
  download.file(file.path("http://54.251.104.13/images",path),'~/Downloads/SM001.png')
  img <- readPNG('~/Downloads/SM001.png')
  grid::grid.raster(img)
}

ques<-function(pattern=NULL,w=60){
  ta("questions") %>%
     filter(grepl(pattern,question_name)) %>%
     mutate(ques=str_trunc(question_name,width=w,side="center")) %>%
     select(question_id:topic_id,ques,scribble_id,on_the_fly)
    # merge(ta("uploaded_images"),by.x="scribble_id",by.y="image_id") %>%
    # select(scribble_id:uploaded_by) %>%
    # arrange(desc(question_id))
}

queries<-function(n=10){
    script<-paste("select * from student_query order by query_id desc limit", n)
    q<-suppressWarnings(querysql(script))
    q
}


changestate<-function(user=NULL,state_user=NULL,session=NULL,state_session=NULL){
    if(all(!is.null(user),!is.null(state_user))){
      old_user_state<-user_status(user)$user_state
    string1<-paste("Existing user state:",old_user_state)
    string2<-paste("New user state:",state_user)
    script<-paste("UPDATE tbl_auth SET user_state=",state_user,"WHERE user_id=",user,"LIMIT 1")
    n<-runsql(script)
    if (n==1) {print(string1); print(string2)} else print("No state changes affected on user")
    }
    if(all(!is.null(session),!is.null(state_session))){
        old_sess_state<-show_sessions(days = 7) %>% filter(class_session_id==session) %>% select(session_state) %>% .[,1]
        string1<-paste("Existing session state:",old_sess_state)
        string2<-paste("New session state:",state_session)
        script<-paste("UPDATE class_sessions SET session_state=",state_session,"WHERE class_session_id=",session,"LIMIT 1")
        n<-runsql(script)
        if (n==1) {print(string1); print(string2)} else print("No state changes applied on Session")
    }
}

delete_session<-function(session_id=stop("you have to provide the numeric session_id")){
    script<-paste("DELETE from class_sessions where class_session_id=",session_id)
    deleted_sessions<-runsql(script,database_name)
    message(deleted_sessions,":rows deleted from class_session_id")
}


list_users<-function(school="ALL") {
  if(NROW(school)==1 && school=="ALL") script<-paste("SELECT * FROM tbl_auth") else if (valid_school(school))
        script<-paste("SELECT * FROM tbl_auth WHERE school_id=",school) else return("INVALID school_id")
  x<-suppressWarnings( querysql(script))
  x
}

state_transitions<-function(numbers=10){
    refresh("st_tr100",time_gap_hours = 0)->>st_tr100
    as.tbl(st_tr100) %>% arrange(desc(transition_time)) %>% print(n=numbers)
}



delete_log<-function(time=NULL){
    script<-"DELETE FROM state_transitions WHERE transition_time= ?t LIMIT 1"
    sql<-sqlInterpolate(ANSI(),script,t=time)
    runsql(sql)
}

password_mismatches<-function() {
    xmpp_users<-list_XMPP_users()
    users<-list_users()
    merge(xmpp_users,users,all=TRUE,by.x="username",by.y="user_id") %>% filter(!password.x==password.y) %>%
        select(5,1:2,6,3:4,7)
}


password_sync<-function() {
    x<-password_mismatches()
    sql_text<-paste0("UPDATE users SET password=", "'" ,x$password.y, "'"  ,  " WHERE username=", x$username, ";")
    number_updated<-runsql(sql_text,"ejabberd_1609")
     cat(paste(number_updated,"password(s) updated in XMPP server"))
    #cat(sql_text)
}

users_missed<-function() {
    x<-list_XMPP_users()
    y<-list_users()
    a<-setdiff(x,y)
        select(5,1:2,6,3:4,7)
}

topics_inClass<-function(class_id=14,tag=1){
    ifelse(tag<2,
           sql_text<-paste0("SELECT * FROM topic where topic_id in (select topic_id from lesson_plan where class_id=",class_id," and topic_tagged=",tag,")"),
           sql_text<-paste0("SELECT * FROM topic where topic_id in (select topic_id from lesson_plan where class_id=",class_id, ")")
    )

    t<-suppressWarnings(  querysql(sql_text) )
    t2<-t
    suppressWarnings( merge(t,t2,by.x = "parent_topic_id",by.y = "topic_id")) ->t3
    t3[,!duplicated(colnames(t3))] %>% select(13,1,5,2,8:10) %>% rename(Main_topic=topic_name.y,Maintopic_id=parent_topic_id,SubTopic=topic_name.x,SubTopic_id=topic_id,GI_num=gi_num.x,GI_den=gi_den.x,PI=pi.x)

}

main_topics_inClass<-function(class_id=14,tag=1){
    topics_inClass(class_id,tag=tag) %>% group_by(Main_topic) %>% tally %>% rename(Count_SubTopics=n)
}

main_topics<-function(subpat=NULL,toppat=NULL){
    tt<-c(is.null(subpat),is.null(toppat))
    t<-suppressWarnings(querysql("SELECT * from topic"))
    s<-suppressWarnings(querysql("SELECT * from subjects"))
    if(identical(tt,c(T,T))) t %>% filter(is.na(parent_topic_id)) %>% merge(s) %>% rename(MainTopic=topic_name) %>% select(subject_name,subject_id,MainTopic,topic_id) %>% arrange(subject_id,topic_id) else
        if(identical(tt,c(T,F))) t %>% filter(is.na(parent_topic_id)) %>% merge(s) %>% rename(MainTopic=topic_name) %>% select(subject_name,subject_id,MainTopic,topic_id) %>% filter(grepl(toppat,MainTopic,ignore.case = T)) %>% arrange(subject_id,topic_id) else
        if(identical(tt,c(F,T))) t %>% filter(is.na(parent_topic_id)) %>% merge(s) %>% rename(MainTopic=topic_name) %>% select(subject_name,subject_id,MainTopic,topic_id) %>% filter(grepl(subpat,subject_name,ignore.case = T)) %>% arrange(subject_id,topic_id) else
        if(identical(tt,c(F,F))) t %>% filter(is.na(parent_topic_id)) %>% merge(s) %>% rename(MainTopic=topic_name) %>% select(subject_name,subject_id,MainTopic,topic_id) %>% filter(grepl(subpat,subject_name,ignore.case = T),grepl(toppat,MainTopic,ignore.case = T)) %>% arrange(subject_id,topic_id)
}

subtopics_all<-function(){
    t<-suppressWarnings(querysql("SELECT * from topic"))
    s<-suppressWarnings(querysql("SELECT * from subjects"))
    t2<-t
    suppressWarnings( merge(t,t2,by.x = "parent_topic_id",by.y = "topic_id")) ->t3
    t3[,!duplicated(colnames(t3))] %>% select(13,1,5,2,8:10) %>% rename(Main_topic=topic_name.y,Maintopic_id=parent_topic_id,SubTopic=topic_name.x,SubTopic_id=topic_id,GI_num=gi_num.x,GI_den=gi_den.x,PI=pi.x)
}

topics<-function(pattern=NULL,topicid=NULL){
    dtopics<-refresh("dtopics")
    dclasses<-refresh("dclasses")
    dlesson_plan<-refresh("dlesson_plan",time_gap_hours = 0.1)
    dsubjects<-refresh("dsubjects")
    tt<-c(is.null(pattern),is.null(topicid))
    
    if(identical(tt,c(F,T))){
        x<-dtopics %>% 
            filter(grepl(pattern,topic_name,ignore.case=T))
    } else
        if(identical(tt,c(T,F))) 
            x<- dtopics %>% 
        filter(topic_id==topicid) 
    else
        return("Error")
    output<-x %>% inner_join(dsubjects,by="subject_id") %>% 
        left_join(dlesson_plan,by="topic_id") %>%
        left_join(dclasses,by="class_id") %>% 
        select(topic_id,topic_name,parent_topic_id,class_id,class_name,class_teacher=teacher_id,subject_id=subject_id.x,subject_name,class_subject=subject_id.y) %>% 
        left_join(dsubjects[,c("subject_id","subject_name")],by=c("class_subject"="subject_id")) %>% 
        left_join(dtopics[,c("topic_id","topic_name")],by=c("parent_topic_id"="topic_id")) %>% 
        rename("parent_topic"="topic_name.y","topic_name"="topic_name.x","topic_subj_name"="subject_name.x","class_subj
               _name"="subject_name.y") %>% 
        select(topic_id:parent_topic_id,parent_topic,everything())
    return(output)
}

cols<-function(table="class_sessions", dbname=database_name,exact=F){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address,port=port)  # connect the DB
    on.exit(dbDisconnect(db))
    if(!exact && NROW(x<-grep(table,tabs,ignore.case = T,value=T))==1)  dbListFields(db,x) else 
        if(exact) dbListFields(db,table) else if(NROW(x)>1) "No unique match" else "Not a single matching table found" 
}

table_head<-function(table="class_sessions", nrow=20,dbname=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address,port=port)  # connect the DB
    on.exit(dbDisconnect(db))
    suppressWarnings(    dbReadTable(db,table)) %>% head(nrow)
}


disconnect<-function(dbhandler=1){
    dbDisconnect(dbListConnections(MySQL())[[dbhandler]])
}



insert_new_subtopic<-function(parent_topic_id='NULL', subject_id=stop("subject_id is mandatory"), topic_name=stop("new subtopic name is mandatory"),topic_info="This is added from r console"){
query_text<-sprintf("INSERT INTO topic (parent_topic_id,subject_id,classify_id,topic_name,topic_info,gi_num,gi_den,pi) VALUES(%d,%d,%d,'%s','%s',%d,%d,%d)",parent_topic_id,subject_id,1,topic_name,topic_info,0,0,0)
x<-runsql(query_text)
if(x==1) new_topic_id<-querysql(sprintf("select topic_id from topic where topic_name='%s'",topic_name))
if (nrow(new_topic_id)>1) return("you have two identical topic names, and may want to change one") else
return(new_topic_id$topic_id)    
}


show_sessions<-function(user="ALL",class="ALL", days=1,limit=30){
  India<-"Asia/Kolkata"
  state<-"NONE"
  if(all(user=="ALL",class=="ALL")) state="ALL" else
  { r<-role(user); 
  if (length(r)==1 && r==3) state="TEACHER" else
      if (length(r)==1 && r==4) state="STUDENT" else
          if (valid_class(class)) state="CLASS" else 
              return("Invalid 'user'")
  }
  script<-switch(EXPR = state,
                 "ALL"="SELECT * from class_sessions WHERE DATE(starts_on)> DATE_SUB(NOW(), INTERVAL ?da DAY) ORDER BY starts_on DESC LIMIT ?li",
                 "TEACHER"="SELECT * from class_sessions WHERE teacher_id=?t AND DATE(starts_on)>DATE_SUB(NOW(), INTERVAL ?da DAY) ORDER BY starts_on DESC LIMIT ?li",
                 "STUDENT"="SELECT * from class_sessions WHERE DATE(starts_on)>DATE_SUB(NOW(), INTERVAL ?da DAY)",
                 "CLASS"="SELECT * from class_sessions WHERE DATE(starts_on)>DATE_SUB(NOW(), INTERVAL ?da DAY) AND class_id =  ?ci ORDER BY starts_on DESC LIMIT ?li",
                 "ERROR"
  )
  if(state=="STUDENT") 
    script<-sqlInterpolate(ANSI(),script,da=days)
  
  if(state=="CLASS") 
    script<-sqlInterpolate(ANSI(),script,da=days,ci=class,li=limit)
  
  if(state=="TEACHER") 
    script<-sqlInterpolate(ANSI(),script,t=user, da=days,li=limit)
  
  if(state=="ALL") 
    script<-sqlInterpolate(ANSI(),script,da=days,li=limit)
  
  if(!state =="ERROR"){
    x<-querysql(script)
     x$starts_on %<>% ymd_hms(tz = India)
     x$ends_on %<>% ymd_hms(tz=India)
  } 
  if(state=="STUDENT") {
    x %<>% filter(class_id %in% classes(user = user,detail=F))
  } else "ERROR"
  x
}


user_status<-function(users=monitored_users) {
  list_users() %>% filter(user_id %in% users) %>% select(c(1,2,4:11)) ->  output
  monitored_users<<-users
  output
}

volunteer_sessions<-function(days=100){
x<-queries(n=days)
x %>% filter(query_id %in% x$query_id,votes_received>0,allow_volunteer==1)
}




#---the following functions are called from all the other functions for database interactions.


table_all<-function(table=NULL,dbname=database_name,exact=F){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=dbname,
                       host=host_address,port=port)  # connect the DB
    on.exit(dbDisconnect(db))
    if(!exact && NROW(x<-grep(table,tabs,ignore.case = T,value=T))==1)  suppressWarnings(dbReadTable(db,x)) else 
        if(exact) suppressWarnings(dbReadTable(db,table)) else if (NROW(x)>1) "No unique match" else "No match found"
}


runsql<-function(sql_text=stop("Please provide an SQL script starting with INSERT or UPDATE as first parameter and a database name as second. If database = 'jupiter_dev' you can skip"),database=database_name){
    db<-dbConnect(MySQL(),user=db_user_name,password=db_password,dbname=database,
                  host=host_address,port=port)  # connect the DB
    on.exit(dbDisconnect(db))
    rows_affected<-dbExecute(db,sql_text)
    rows_affected
    }


querysql<-function(sql_text=NULL,database=database_name){
    db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=database,
                  host=host_address,port=port)  # connect the DB
    on.exit(dbDisconnect(db))
    dbGetQuery(db,sql_text)
}

writedbtable<-function(df=NULL,tablename=NULL,database=database_name,overwrite_flag=F){
  db<-DBI::dbConnect(RMySQL::MySQL(),user=db_user_name,password=db_password,dbname=database,
                     host=host_address,port=port)  # connect the DB at Amazon
  on.exit(dbDisconnect(db))
  dbWriteTable(db, tablename,df,overwrite=overwrite_flag,row.names = FALSE)
}

log<-function(api=NULL,user=NA,parameters=NA,returned_value=NULL){
    INDIA="Asia/Kolkata"
    #api<-deparse(sys.calls()[[sys.nframe()-1]]) - this line does not work
    write.table(data.frame(time=now(tzone=INDIA),API=api,user_id=user,Parameters=parameters),quote=F,file="/var/www/logs/api.logs.txt",row.names=F, col.names=F, append=T,sep=";")
}


update_event_log<-function(db,service_name='UNKNOWN', user_id=0,      UUID=0,         xml_input='XYZ',    request_time=now(),
                           xml_output='Nothing',   return_time=now()){
    sql <- "INSERT INTO event_log values  (0,?service_name, ?user_id,  ?UUID, ?xml_input, ?request_time, ?xml_output, ?return_time, 'json'  )"
    sql_new<-sqlInterpolate(ANSI(), sql, service_name=service_name, user_id=user_id,      UUID=UUID,         xml_input=xml_input,    request_time=as.character(request_time), xml_output=xml_output,   return_time=as.character(return_time))
    on.exit(dbDisconnect(db))
    x<-dbExecute(db,sql_new)
    x
}

update_state_transition<-function(type=1,id=NULL,from=NULL,to=NULL,time=as.character(NOW())){
    sql <- "INSERT INTO state_transitions VALUES (?a, ?b, ?c, ?d, ?t)"
    sql_new<-sqlInterpolate(ANSI(), sql, a=type,b=id,c=from,d=to,t=time)
    on.exit(dbDisconnect(db))
    rows_inserted<-dbExecute(db,sql_new)
    table_head("state_transitions") %>% filter(transition_time==time)
   
}

fill_missing_time<-function(table_name=NULL,column_name=NULL){
  if(table_name=="student_index"){
    st_index %>% filter(is.na(last_updated)) %>% count(class_session_id) %>% left_join(sessions100) %>% filter(complete.cases(class_id)) ->x1
    y1<-sprintf("update student_index set last_updated=TIMESTAMP('%s') where class_session_id=%d LIMIT %d",x1$ends_on,x1$class_session_id,x1$n)
    print(paste(lapply(y1,runsql),"change(s) done for ",x1$class_session_id))
    message(sprintf("%d:total changes in DB",sum(x1$n)))
  }
  
  
}

search_classes<-function(topic_id=NULL){
}

search_functions<-function(string=str){grep(string,list_of_functions(),value=T)}

#-----declare alias or short names
sf<-search_functions
ta<-table_all
ss<-show_sessions
qs<-querysql
st<-subtopics_all
mt<-main_topics
tic<-topics_inClass
rq<-replace_question
lu<-list_users
us<-user_status
ms<-move_school
sc<-start_classes
stud<-students
di<-display_image
lv<-live_status
vs<-volunteer_sessions

disconnect()