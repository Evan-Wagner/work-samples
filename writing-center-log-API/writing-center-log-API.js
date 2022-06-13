
const MILLIS_PER_DAY = 1000*60*60*24;
const MILLIS_PER_HALF_HOUR = 1000*60*30;

// Change the inputs to the constants below when you move files around, change their layouts, reorder columns, etc.
// so you don't have to go through the code and change every line!

// folders
const folderSpring22 = DriveApp.getFolderById('*********************************');

const folderRemoteSignupForms = DriveApp.getFolderById('*********************************');
const folderSubForms = DriveApp.getFolderById('*********************************');
const folderCancelForm = DriveApp.getFolderById('*********************************');
const folderBackups = DriveApp.getFolderById('*********************************');

// the spreadsheet named "Writing Center Sign-Up S22"
const ss = SpreadsheetApp.getActiveSpreadsheet();
const ssLink = ss.getUrl();

// sheet that displays form links for remote signups for the current week and the next week
const sheetSignup = ss.getSheetByName('Sign Up');

// ranges in sheetSignUp
const rangeThisWeekRemote = sheetSignup.getRange('B2:H18');
const rangeThisWeekInPerson = sheetSignup.getRange('B20:H36');
const rangeNextWeekRemote = sheetSignup.getRange('J2:P18');
const rangeNextWeekInPerson = sheetSignup.getRange('J20:P36');

// sheet that displays the weekly schedule, copied to sheetSignUp when sign-up forms are refreshed.
// That should happen on Sunday when the writing center is closed.
const sheetSchedule = ss.getSheetByName('Schedule');

// ranges in sheetSchedule
const rangeStaticRemote = sheetSchedule.getRange('A1:G17');
const rangeStaticInPerson = sheetSchedule.getRange('A19:G35');

// sheet that logs form responses
const sheetLog = ss.getSheetByName('Log');            

// column indexes in sheetLog
const colTimestamp = 1;
const colMode = 2;
const colDate = 3;
const colTime = 4;
const colStuName = 5;
const colStuEmail = 6;
const colConsName = 7;
const colConsEmail = 8;
const colDept = 9;
const colLevel = 10;
const colProf = 11;
const colSignupForm = 12;
const colEvent = 13;
const colVideo = 14;
const colCancelForm = 15;
const colStatus = 16;

// sheet that displauys a list of consultant names and emails, the sub request form and sub confirm forms, liaison names and other info
const sheetInput = ss.getSheetByName('Inputs');

// column indexes in sheetInput
const colConsNameInput = 1;
const colConsEmailInput = 2;
const colDeptInput = 3;
const colLevelInput = 4;
const colSubFormsInput = 5;

const colInPersonFormInput = 7;
const colLiasNameInput = 8;
const colLiasEmailInput = 9;
const colLiasCourseInput = 10;
const colLiasProfInput = 11;
const colLiasCombinedInput = 12;


// ***********
//  functions
// ***********


// takes sheet, column index, and optional row indices; returns list of every cell value in that column inside those rows
function getColumnAsList(sheet, col, firstRow = 2, lastRow = sheet.getLastRow()) { 
  var list = [];
  for (var i=firstRow; i<=lastRow; ++i) {
    var item = sheet.getRange(i, col).getValue();
    if (item!="") {
      list[i-firstRow] = item;
    }
  }
  return list;
}


// takes a single-cell range; finds the name it contains, its time row, and its day of the week column;
// creates a sign-up form based on that information; puts the form's link in the input cell
function createRemoteSignUpForm(cell) {
  if (cell.isBlank()==false && cell.getRichTextValue().getLinkUrl()==null) {
    var consName = cell.getValue();
    var day = sheetSignup.getRange(2,cell.getColumn()).getDisplayValue();
    var time = sheetSignup.getRange(cell.getRow(),2).getDisplayValue();

    var form = FormApp.create(day+" at "+time+" ("+consName+")")
      .setDescription("");
    DriveApp.getFileById(form.getId()).moveTo(folderRemoteSignupForms);

    form.addTextItem()
      .setTitle("What is your name? (i.e. John Green)")
      .setRequired(true);
    form.addListItem()
      .setTitle("Which department, if any, is this piece of writing for?")
      .setChoiceValues(getColumnAsList(sheet=sheetInput,col=colDeptInput))
      .setRequired(true);
    form.addMultipleChoiceItem()
      .setTitle("Course level?")
      .setChoiceValues(getColumnAsList(sheet=sheetInput,col=colLevelInput))
      .setRequired(true);
    form.addMultipleChoiceItem()
      .setTitle("Your professor's last name? (i.e. Griggs)")
      .setChoiceValues(["Not for a class or professor"])
      .showOtherOption(true)
      .setRequired(true);
    form.addMultipleChoiceItem()
      .setTitle("Just to confirm, you want to sign up for an appointment on "+day+" at "+time+" with "+consName+", correct?")
      .setChoiceValues(["Yep"])
      .setRequired(true);

    form.setConfirmationMessage("Thanks for signing up! Check your email for a Google Calendar invitation in the next few minutes.")
      .setShowLinkToRespondAgain(false)
      .setCollectEmail(true);

    style = SpreadsheetApp.newTextStyle()
      .setForegroundColor('#9d48e0')
      .setUnderline(true)
      .build();
    linkText = SpreadsheetApp.newRichTextValue()
      .setText(consName)
      .setLinkUrl(form.getEditUrl())
      .setTextStyle(style)
      .build(); 
    cell.setRichTextValue(linkText);
  }
}


// takes a JavaScript Date object; returns a string "[day of the week] [day of the month]/[month] at [time]"
function formatDate(date, includeTime=false) {
  var weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  var weekday = weekdays[date.getDay()];
  var month = date.getMonth()+1;
  if (month<10) {month = "0"+month;}
  var day = date.getDate();
  if (day<10) {day = "0"+day;}
  var string = weekday+" "+month+"/"+day;

  if (includeTime==true) {
    var minutes = date.getMinutes();
    if (minutes<10) {minutes = "0"+minutes;}
    var hours = date.getHours();
    if (hours<13) {
      string = string.concat(" at "+hours+":"+minutes+" AM");
    } else {
      string = string.concat(" at "+(hours-12)+":"+minutes+" PM");
    }
  }

  return string;
}


// takes a date string; returns a Date object. Basically this and formatDate() reverse each other
function deformatDate(formattedDate) {
  var weekday = formattedDate.substr(0, 3);
  var month = formattedDate.substr(4, 2);
  var day = formattedDate.substr(7, 2);
  var today = new Date();
  var year = today.getYear()+1900;
  var time = "";
  if (formattedDate.length>9) {time = formattedDate.substr(13, formattedDate.length-13);}

  var string = weekday+" "+month+"/"+day+"/"+year+" "+time;
  var date = new Date(string);
  return date;
}


// takes row index in sheetLog; returns list of cell values in that row, plus some other information it finds
function getAppointmentInfo(row) {
  var list = [
    timestamp = sheetLog.getRange(row, colTimestamp).getValue(),
    mode = sheetLog.getRange(row, colMode).getValue(),
    date = sheetLog.getRange(row, colDate).getDisplayValue(),
    time = sheetLog.getRange(row, colTime).getDisplayValue(),
    dateTime = date+" at "+time,
    dateObject = deformatDate(dateTime),

    stuName = sheetLog.getRange(row, colStuName).getValue(),
    stuEmail = sheetLog.getRange(row, colStuEmail).getValue(),
    consName = sheetLog.getRange(row, colConsName).getValue(),
    consEmail = sheetLog.getRange(row, colConsEmail).getValue(),

    dept = sheetLog.getRange(row, colDept).getValue(),
    level = sheetLog.getRange(row, colLevel).getValue(),
    prof = sheetLog.getRange(row, colProf).getValue(),

    linkSignup = sheetLog.getRange(row, colSignupForm).getValue(),
    eventId = sheetLog.getRange(row, colEvent).getValue(),
    linkVideo = sheetLog.getRange(row, colVideo).getValue(),
    linkCancel = sheetLog.getRange(row, colCancelForm).getValue(),
    status = sheetLog.getRange(row, colStatus).getValue()
  ];

  return list;
}


// takes a row index in sheetLog; finds appointment info from that row;
// creates a cancel form based on that information; puts the form's link in a cell of the input row
function createCancelForm(row) {
  getAppointmentInfo(row);

  var form = FormApp.create("Cancel: "+dateTime+" ("+consName+")")
    .setDescription("Fill out this form only if you are "+consName+".")
    .setConfirmationMessage("Thank you for letting us know!")
    .setCollectEmail(true)
    .setShowLinkToRespondAgain(false);
  DriveApp.getFileById(form.getId()).moveTo(folderCancelForm);

  form.addMultipleChoiceItem()
    .setTitle("Confirm your name.")
    .setChoiceValues([consName])
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Do you want to cancel this appointment for "+dateTime+" with "+stuName+"?")
    .setChoiceValues([
      "Yes",
      "No, this appointment happened/will happen but some of the info needs correcting"
    ])
    .setRequired(true);
  form.addParagraphTextItem()
    .setTitle("If you chose the second option, what is the correct info for this appointment?")
    .setRequired(false);

  return form.getEditUrl();
}


// takes row index in sheetLog; finds appointment information in that row; creates a calendar event based on that information;
// invites student and consultant; adds a Hangouts link; puts event ID and Hangouts url in cells of the input row
function createEvent(row) {
  getAppointmentInfo(row);
  if (mode=="remote" && eventId=="") {
    var title = "Online Appointment: "+stuName+" & "+consName;
    var startTime = dateObject;
    var endTime = new Date(startTime.getTime() + MILLIS_PER_HALF_HOUR);

    var start = Utilities.formatDate(startTime, "EST", "yyyy-MM-dd'T'HH:mm:ssZ");
    var end = Utilities.formatDate(endTime, "EST", "yyyy-MM-dd'T'HH:mm:ssZ");

    var details = {
      summary: title, 
      start: {dateTime: start},
      end: {dateTime: end},
      attendees: [
        {email: stuEmail},
        {email: consEmail},
      ],
      conferenceData: {
        createRequest: {
          conferenceSolutionKey: {type: "hangoutsMeet"},
          requestId: "slatfalf",
        },
      },
      reminders: {
        useDefault: false,
        overrides: [
          {
            method: "email",
            minutes: 15
          }
        ]
      },
    };

    var event = Calendar.Events.insert(details, "w************@kenyon.edu", {conferenceDataVersion: 1,});
    var hangoutUrl = event.hangoutLink + (event.hangoutLink.indexOf('?') == -1 ? '?' : '&') + 'authuser=' + consEmail;
    event.description = (event.description || '').replace(/^Hangout(\/Meet)?:.+$[\s]+/mg, 'Hangout/Meet: ' + hangoutUrl + '\n\n');
    Calendar.Events.update(event, "w************@kenyon.edu", event.id);

    sheetLog.getRange(row, colEvent).setValue(event.id);
    sheetLog.getRange(row, colVideo).setValue(hangoutUrl);
  }
}


// takes a row index; finds appointment information in that row;
// sends an email to the student and consultant with appointment details and cancel form link
function sendEmail(row) {
  getAppointmentInfo(row);

  // conditional wording for the body
  var levelMessage = " "+level; if (level=="N/A"||level=="") {levelMessage = "";}
  var deptMessage = " "+dept+" assignment";
  if (dept=="Application, Cover Letter, or Resume") {deptMessage = "n application, cover letter, or resume";}
  if (dept=="This writing isn't for a Kenyon class") {deptMessage = "n unspecified piece of writing";}
  var profMessage = " for Prof. "+prof;
  if (prof=="Not for a class or professor"||prof=="") {profMessage = "";}
  var topicMessage = " They're working on a"+levelMessage+deptMessage+profMessage+".";
  var videoLinkMessage = "";
  if (mode=="remote") {
    videoLinkMessage = "\n\nLink to Google Meet: "+linkVideo;
  }

  // composes the email
  var consEmail = sheetLog.getRange(row, colConsEmail).getValue();
  var subject = "New "+mode+" appointment: "+stuName+" ("+dateTime+")";
  var message = ""+stuName+" has signed up for an appointment with you on "+dateTime+"."+topicMessage+" If necessary, you can reach them at "+stuEmail+"."+videoLinkMessage+"\n\nNeed to cancel this appointment, or correct some of the information that was submitted? Please fill out this form: "+linkCancel;

  // sends the email
  MailApp.sendEmail(consEmail, subject, message);

  // sends student an email if remote
  if (mode=="remote") {
    var consName = sheetLog.getRange(row, colConsName).getValue();
    MailApp.sendEmail(stuEmail, "Appointment: "+dateTime+" with "+consName, "You successfully signed up for a remote appointment with "+consName+" on "+dateTime+". If necessary, you can reach them at "+consEmail+"."+videoLinkMessage);
  }
}

// takes a single-cell range; resets its RichText to not include a link
function removeLink(cell) {
  var str = cell.getValue();
  var newText = SpreadsheetApp.newRichTextValue()
    .setText(str)
    .setTextStyle(null)
    .setLinkUrl(null)
    .build();
  cell.setRichTextValue(newText);
  var style = SpreadsheetApp.newTextStyle()
    .setForegroundColor('black')
    .setUnderline(false)
    .build();
  cell.setTextStyle(style);
}


//takes consultant name; finds it in sheetInput; returns that consultant's email
function getConsEmail(name) {
  var nameList = getColumnAsList(sheet = sheetInput, col = colConsNameInput);
  var email = "email not found";
  for (var i=0; i<nameList.length; ++i) {
    if (name==nameList[i]) {
      email = sheetInput.getRange(i+2, colConsEmailInput).getValue();
      break;
    }
  }

  if (email=="email not found") {
    MailApp.sendEmail("w******@kenyon.edu",name+" is not on the consultant list","!")
  }
  return email;
}


// takes a Date object; returns true if it is before current day
function isBeforeCurrentDate(date) {
  var today = new Date();
  if (date.getTime()<today.getTime()-MILLIS_PER_DAY) {
    return true;
  } else {
    return false;
  }
}


// takes a multi-cell range containing remote signup links; checks for form responses in each link;
// if somebody signed up for an appointment, calls createEvent() to make a calendar appointment and sendEmail() to email the consultant & consultee;
// calls removeLink() to remove the link from the cell;
// if nobody signed up for the appointment but the day of the shift has passed, removes the link
function logRemoteAppointments(range) {
  var numCols = range.getNumColumns();
  var numRows = range.getNumRows();

  for (var i=2; i<=numCols; ++i) {
    for (var j=2; j<=numRows; ++j) {
      var cell = range.getCell(j, i);
      var editUrl = cell.getRichTextValue().getLinkUrl();
      if (editUrl!=null) {
        var date = sheetSignup.getRange(2, cell.getColumn()).getDisplayValue();
        var time = sheetSignup.getRange(cell.getRow(), 2).getDisplayValue();
        
        var form = FormApp.openByUrl(editUrl);
        var responses = form.getResponses();
        if (responses.length>0) {
          var logEntry = [];
          
          logEntry[colMode] = "remote";
          logEntry[colConsName] = cell.getValue();
          logEntry[colConsEmail] = getConsEmail(cell.getValue());
          logEntry[colDate] = date;
          logEntry[colTime] = time;
          logEntry[colSignupForm] = editUrl;

          var response = responses[0];        
          logEntry[colTimestamp] = response.getTimestamp();
          logEntry[colStuEmail] = response.getRespondentEmail();

          var itemResponses = response.getItemResponses();
          logEntry[colStuName] = itemResponses[0].getResponse();
          logEntry[colDept] = itemResponses[1].getResponse();
          logEntry[colLevel] = itemResponses[2].getResponse();
          logEntry[colProf] = itemResponses[3].getResponse();

          sheetLog.insertRowAfter(1);
          for (var k=1; k<logEntry.length; ++k) {
            sheetLog.getRange(2, k).setValue(logEntry[k]);
          }

          sheetLog.getRange(2, colCancelForm).setValue(createCancelForm(row=2));
          createEvent(row=2);
          sendEmail(row=2);
          removeLink(cell);

          while (responses.length>1) {
            response = responses.pop();
            itemResponses = response.getItemResponses();

            var stuName = itemResponses[0].getResponse();
            var stuEmail = response.getRespondentEmail();
            var consName = logEntry[colConsName];
            var dateTime = logEntry[colDate]+" at "+logEntry[colTime];

            MailApp.sendEmail(stuEmail, "The appointment slot on "+dateTime+" has already been taken", "Hi "+stuName+",\n\nUnfortunately, someone else booked an appointment with "+consName+" on "+dateTime+" before you submitted your request. But don't worry, there are plenty more appointments available: "+ssLink);
          }
        }
        if (isBeforeCurrentDate(deformatDate(date))) {removeLink(cell);}
      }
    }
  }
}

// takes a Form; removes all its items
function clearForm(form) {
  while (form.getItems().length>0) {
    form.deleteItem(0);
  }

  return form;
}

// checks the cell that contains the link to the Form for walk-in appointments; if it isn't there, generates a new one
function generateInPersonLogForm() {
  var cell = sheetInput.getRange(1, colInPersonFormInput);
  var editLink = cell.getRichTextValue().getLinkUrl();
  if (editLink==null) {
    var form = FormApp.create("Walk-In & Liaison Sign-In")
      .setIsQuiz(true);
    DriveApp.getFileById(form.getId()).moveTo(folderSpring22);
  } else {
    var form = FormApp.openByUrl(editLink);
    clearForm(form);
  }

  var liasNameList = getColumnAsList(sheetInput, colLiasNameInput);
  var liasCourseList = getColumnAsList(sheetInput, colLiasCourseInput);
  var liasProfList = getColumnAsList(sheetInput, colLiasProfInput);
  for (var i=0; i<liasNameList.length; ++i) {
    if (liasProfList[i]==null) {
      liasProfList[i]="";
    } else {
      liasProfList[i]=" ("+liasProfList[i]+")";
    }
  }
  var liasEmailList = getColumnAsList(sheetInput, colLiasEmailInput);
  var combinedList = [];
  for (var i=0; i<liasNameList.length; ++i) {
    combinedList[i] = liasCourseList[i]+liasProfList[i]+": "+liasNameList[i]+" ["+liasEmailList[i]+"]";
  }

  var consOrLias = form.addMultipleChoiceItem()
    .setTitle("Are you signing into a walk-in or a liaison consultation?")
    .setRequired(true); //choices are added after other form items so they can reroute the user to different pages
  
  var consPage = form.addPageBreakItem()
    .setTitle("Walk-in Appointment");
  form.addTextItem()
    .setTitle("What is your name? (i.e. John Green)")
    .setRequired(true);
  form.addTextItem()
    .setTitle("What is your Kenyon email address?")
    .setRequired(true);
  form.addDateTimeItem()
    .setTitle("What is the starting time of this appointment?")
    .setRequired(true);
  form.addListItem()
    .setTitle("Which writing consultant is helping you?")
    .setChoiceValues(getColumnAsList(sheetInput, colConsNameInput))
    .setRequired(true);
  form.addListItem()
    .setTitle("Which department, if any, is this piece of writing for?")
    .setChoiceValues(getColumnAsList(sheetInput, colDeptInput))
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Course level?")
    .setChoiceValues(getColumnAsList(sheetInput, colLevelInput))
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Your professor's last name? (i.e. Griggs)")
    .setChoiceValues(["Not for a class or professor"])
    .showOtherOption(true)
    .setRequired(true);

  var liasPage = form.addPageBreakItem()
    .setGoToPage(FormApp.PageNavigationType.SUBMIT)
    .setTitle("Liaison Appointment");
  form.addTextItem()
    .setTitle("What is your name? (i.e. John Green)")
    .setRequired(true);
  form.addTextItem()
    .setTitle("What is your Kenyon email address?")
    .setRequired(true);
  form.addDateTimeItem()
    .setTitle("What is the starting time of this appointment?")
    .setRequired(true);
  form.addListItem()
    .setTitle("Which course is this liaison appointment for?")
    .setChoiceValues(getColumnAsList(sheetInput, colLiasCombinedInput))
    .setRequired(true);

  form.addPageBreakItem()
    .setGoToPage(FormApp.PageNavigationType.SUBMIT);
  form.addMultipleChoiceItem()
    .setChoiceValues(["Response logged"]);
  
  consOrLias.setChoices([
    consOrLias.createChoice("walk-in", consPage),
    consOrLias.createChoice("liaison", liasPage)
  ])
  
  form.setConfirmationMessage("Thanks, and happy writing!")
    .setShowLinkToRespondAgain(true)
    .setCollectEmail(false);

  var richText = SpreadsheetApp.newRichTextValue()
    .setText("In-Person Form")
    .setLinkUrl(form.getEditUrl())
    .build();
  cell.setRichTextValue(richText);
}

// checks the walk-in Form for responses; logs each one and marks it as viewed (using the quiz grading feature of Forms)
function logInPersonAppointments() {
  var cell = sheetInput.getRange(1, colInPersonFormInput);
  var editLink = cell.getRichTextValue().getLinkUrl();
  if (editLink==null) {generateInPersonLogForm();}
  else {
    var form = FormApp.openByUrl(editLink);
    var responses = form.getResponses();
    var responseLogged = 0;
    while (responses.length>0) {
      var response = responses.pop();
      var itemResponses = response.getItemResponses();
      var lastResponse = response.getGradableItemResponses().pop();
      responseLogged = lastResponse.getScore();

      if (responseLogged==1) {
        continue;
      } else {
        var logEntry = [];
        var timestamp = response.getTimestamp();
        logEntry[colTimestamp] = timestamp;
        logEntry[colMode] = itemResponses[0].getResponse();
        logEntry[colStuName] = itemResponses[1].getResponse();
        logEntry[colStuEmail] = itemResponses[2].getResponse();

        var date = new Date(itemResponses[3].getResponse());
        logEntry[colDate] = formatDate(date);
        if (date.getHours()==12) {
          logEntry[colTime] = date.getHours()+":00 PM";
        } else if (date.getHours()>12) {
          logEntry[colTime] = (date.getHours()-12)+":00 PM";
        } else {
          logEntry[colTime] = date.getHours()+":00 AM";
        }

        if (logEntry[colMode]=="walk-in") {
          logEntry[colConsName] = itemResponses[4].getResponse();
          logEntry[colConsEmail] = getConsEmail(logEntry[colConsName]);
          logEntry[colDept] = itemResponses[5].getResponse();
          logEntry[colLevel] = itemResponses[6].getResponse();
          logEntry[colProf] = itemResponses[7].getResponse();
        } else {
          var combinedList = getColumnAsList(sheetInput, colLiasCombinedInput);
          var combinedChoice = itemResponses[4].getResponse();
          for (var i=0; i<combinedList.length; ++i) {
            if (combinedList[i]==combinedChoice) {
              logEntry[colConsName] = sheetInput.getRange(i+2, colLiasNameInput).getValue();
              logEntry[colConsEmail] = sheetInput.getRange(i+2, colLiasEmailInput).getValue();
              logEntry[colDept] = sheetInput.getRange(i+2, colLiasCourseInput).getValue();
              logEntry[colProf] = sheetInput.getRange(i+2, colLiasProfInput).getValue();
              break;
            }
          }
        }

        sheetLog.insertRowAfter(1);
        for (var k=1; k<logEntry.length; ++k) {
          sheetLog.getRange(2, k).setValue(logEntry[k]);
        }

        sheetLog.getRange(2, colCancelForm).setValue(createCancelForm(row=2));
        sendEmail(row=2);

        lastResponse.setScore(1);
        response.withItemGrade(lastResponse);
        form.submitGrades([response]);
      }
    }
  }
}

// takes a Date object and consultant name; finds the cell containing the associated appointment
function findShift(dateTime, consName) {
  var cell;
  var tf = ss.createTextFinder(consName);
  var all = tf.findAll();
  for (var i in all) {
    try {
      var thisDate = sheetSignup.getRange(2, all[i].getColumn()).getValue();
      var thisTime = sheetSignup.getRange(all[i].getRow(), 2).getValue();
    } catch(err) {
      continue;
    }

    if (thisDate.getDate()==dateTime.getDate()
    && thisDate.getMonth()==dateTime.getMonth()
    && thisTime.getHours()==dateTime.getHours()) {
      cell = all[i];
      break;
    }
  }

  return cell;
}

// checks each cancel form; if response found, marks the appointment as "CANCELED" or "INFO WRONG"
function checkForCancels() { 
  for (var row=2; row<=sheetLog.getLastRow(); ++row) {
    getAppointmentInfo(row);
    if (status=="" && linkCancel!="") {
      var cancelForm = FormApp.openByUrl(linkCancel);
      var responses = cancelForm.getResponses();
      while (responses.length>0) {
        var response = responses.pop();
        var itemResponses = response.getItemResponses();
        var consName = itemResponses[0].getResponse();
        var consEmail = response.getRespondentEmail();
        if (consEmail!=getConsEmail(consName)) {
          MailApp.sendEmail(consEmail, "Error processing your appointment cancellation", "You recently submitted a cancel form using this email address, but the name you selected wasn't yours. Please make sure this is your Kenyon email and try again: "+form.getPublishedUrl());
        } else {
          var itemResponses = response.getItemResponses();
          if (itemResponses[1].getResponse()!="Yes") {
            sheetLog.getRange(row, colStatus).setValue("INFO WRONG");
          } else {
            sheetLog.getRange(row, colStatus).setValue("CANCELED");
            
            try {
              Calendar.Events.remove("w************@kenyon.edu",eventId);
            } catch {}

            MailApp.sendEmail(consEmail+", "+stuEmail, "Appointment Canceled",
                              consName+" has canceled your appointment on "+dateTime+".");

            if (!isBeforeCurrentDate(deformatDate(dateTime))) {
              createRemoteSignUpForm(findShift(deformatDate(dateTime), consName));
            }
          }
        }
      }
    }
  }
}

// checks the cell containing the link to the substitute form; if it isn't there, generates a new one
function generateSubForm() {
  var cell = sheetInput.getRange(1, colSubFormsInput);
  var editLink = cell.getRichTextValue().getLinkUrl();
  if (editLink==null) {
    var form = FormApp.create('Shift Substitute Request Form')
      .setDescription("1) Email w******************@kenyon.edu and offer your shift.\n2) Someone will reply all to claim it.\n3) Fill out this form.")
      .setIsQuiz(true);
    DriveApp.getFileById(form.getId()).moveTo(folderSpring22);
  } else {
    var form = FormApp.openByUrl(editLink);
    clearForm(form);
  }

  var consList = getColumnAsList(sheetInput, colConsNameInput);

  form.addListItem()
    .setTitle("Who are you?")
    .setChoiceValues(consList)
    .setRequired(true);
  form.addDateTimeItem()
    .setTitle("Date and time of the shift?")
    .setHelpText("If remote: Just put in the start time of your hour-long shift (e.g. 2:00 PM) and both 30-minute appointment slots (e.g. 2:00 and 2:30) will be transferred to the substitute consultant.")
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Remote or in-person?")
    .setChoiceValues(["remote","in-person"])
    .setRequired(true);
  form.addListItem()
    .setTitle("Who will substitute for you during this shift?")
    .setChoiceValues(consList)
    .setRequired(true);

  form.addPageBreakItem()
    .setGoToPage(FormApp.PageNavigationType.SUBMIT);
  form.addMultipleChoiceItem()
    .setChoiceValues(["Response logged"]);

  form.setConfirmationMessage("Thanks! Your substitute must confirm this switch by filling out a separate form, which will be emailed to them within the next few minutes.")
    .setShowLinkToRespondAgain(true)
    .setCollectEmail(true);

  var richText = SpreadsheetApp.newRichTextValue()
    .setText("Sub Form")
    .setLinkUrl(form.getEditUrl())
    .build();
  cell.setRichTextValue(richText);

  form.deleteAllResponses();
}

// takes info related to a shift someone wants to sub in for: a Date object, whether the shift is remote or in-person, and the substitute's name;
// creates a form for the sub to confirm that they will take over the shift
function createSubConfirmForm(dateTime, mode, subName) {
  var formattedDateTime = formatDate(dateTime, includeTime=true);
  var form = FormApp.create("Confirm Substitution: "+formattedDateTime)
    .setDescription("Fill out this form only if you are "+subName+".");
  DriveApp.getFileById(form.getId()).moveTo(folderSubForms);

  form.addMultipleChoiceItem()
    .setTitle("Confirm your name.")
    .setChoiceValues([subName])
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Confirm the date and time of the shift you are taking over.")
    .setChoiceValues([formattedDateTime])
    .setRequired(true);
  form.addMultipleChoiceItem()
    .setTitle("Confirm remote or in-person.")
    .setChoiceValues([mode])
    .setRequired(true);
  form.setConfirmationMessage("Thanks!")
  .setShowLinkToRespondAgain(false)
  .setCollectEmail(true);

  return form.getEditUrl();
}

// checks the sub form for responses; for each new request, calls createSubConfirmForm(); adds the link to the confirm form column;
// emails the confirm form link to the substitute; marks the response as read
function checkForSubRequests() {
  var editUrl = sheetInput.getRange(1, colSubFormsInput).getRichTextValue().getLinkUrl();
  if (editUrl==null) {
    generateSubForm();
    Logger.log("No sub form existed, so a new one was generated.");
  } else {
    var form = FormApp.openByUrl(editUrl);
    var responses = form.getResponses();
    var responseLogged = 0;
    while (responses.length>0) {
      var response = responses.pop();
      var itemResponses = response.getItemResponses();
      var lastResponse = response.getGradableItemResponses().pop();
      responseLogged = lastResponse.getScore();
      if (responseLogged==1) {
        continue;
      }

      var consName = itemResponses[0].getResponse();
      var consEmail = response.getRespondentEmail();

      if (consEmail!=getConsEmail(consName) && consEmail!="w************@kenyon.edu") {
        MailApp.sendEmail(consEmail, "Error processing your substitution", "You recently submitted a shift substitute form using this email address, but the name you selected wasn't yours. Please make sure this is your Kenyon email and try again: "+form.getPublishedUrl());
      } else {
        var dateTime = new Date(itemResponses[1].getResponse());
        if (findShift(dateTime, consName)==null) {
          MailApp.sendEmail(consEmail, "Error processing your substitution", "You recently submitted a shift substitute form using this email address, but we couldn't find the shift you entered, or your name wasn't on it.\n\nThe sign-up schedule ("+ssLink+") refreshes every Sunday. If this shift isn't on the schedule yet, you must wait until it is to request a substitute.\n\nOtherwise, please try again: "+form.getPublishedUrl());
        } else {
          var mode = itemResponses[2].getResponse();
          var subName = itemResponses[3].getResponse();
          var link = createSubConfirmForm(dateTime, mode, subName);

          var list = getColumnAsList(sheetInput, colSubFormsInput);
          var row = list.length+2;
          var cell = sheetInput.getRange(row, colSubFormsInput);

          var richText = SpreadsheetApp.newRichTextValue()
            .setText(consName)
            .setLinkUrl(link)
            .build();
          cell.setRichTextValue(richText);

          var subEmail = getConsEmail(subName);
          MailApp.sendEmail(subEmail, consName+" has offered you their shift on "+formatDate(dateTime, includeTime=true),"Please fill out the form below to take over this shift:\n\n"+link);
        }
      }

      lastResponse.setScore(1);
      response.withItemGrade(lastResponse);
      form.submitGrades([response]);
    }
  }
}

// takes a Date object; returns true if there is an appointment associated with that date and time
function hasAppointment(thisDateObject) {
  var hasAppointment = false;
  for (var row=2; row<=sheetLog.getLastRow(); ++row) {
    var date = sheetLog.getRange(row, colDate).getDisplayValue();
    var time = sheetLog.getRange(row, colTime).getDisplayValue();
    var dateObject = deformatDate(date+" at "+time);

    if (thisDateObject.getTime()==dateObject.getTime()) {
      hasAppointment = true;
      break;
    }
  }
  return hasAppointment;
}

// checks each link in the confirm form column; if it has a correct response, finds the shift & replaces the original consultant's name with the new one;
// if the shift is remote, calls createRemoteSignUpForm(); if someone already signed up for an appointment for that shift, emails them to let them know;
// clears the cell with the confirm form link
function checkForSubConfirms() {
  var list = getColumnAsList(sheetInput, colSubFormsInput);
  for (var i=0; i<list.length; ++i) {
    var cell = sheetInput.getRange(i+2, colSubFormsInput);
    var richText = cell.getRichTextValue();
    var editUrl = richText.getLinkUrl();
    if (editUrl!=null) {
      var responses = FormApp.openByUrl(editUrl).getResponses();
      while (responses.length>0) {
        var response = responses.pop();
        var itemResponses = response.getItemResponses();
        var subName = itemResponses[0].getResponse();
        var subEmail = response.getRespondentEmail();

        if (subEmail!=getConsEmail(subName)) {
          MailApp.sendEmail(subEmail, "Error processing your substitute confirmation", "You recently submitted a shift substitute confirmation using this email address, but the name you selected wasn't yours. Please make sure this is your Kenyon email and try again: "+form.getPublishedUrl());
        } else {
          var consName = richText.getText();
          var mode = itemResponses[2].getResponse();          
          
          var dateTime1 = deformatDate(itemResponses[1].getResponse());
          var shift1 = findShift(dateTime1, consName);
          shift1.clearContent().setValue(subName);
          if (mode=="remote" && !hasAppointment(dateTime1)) {
            createRemoteSignUpForm(shift1);
          }

          if (mode=="remote") {
            var dateTime2 = new Date(dateTime1.getTime()+MILLIS_PER_HALF_HOUR);
            var shift2 = findShift(dateTime2, consName);
            shift2.clearContent().setValue(subName);
            if (!hasAppointment(dateTime2)) {
              createRemoteSignUpForm(shift2);
            }
          }

          for (var k=2; k<=sheetLog.getLastRow(); ++k) {
            var thisDateTime = sheetLog.getRange(k, colDate).getDisplayValue()+" at "+sheetLog.getRange(k, colTime).getDisplayValue();
            if (thisDateTime==formatDate(dateTime1, includeTime=true) || (dateTime2!=undefined && thisDateTime==formatDate(dateTime2, includeTime=true))) {
              var thisConsName = sheetLog.getRange(k, colConsName).getValue();
              if (thisConsName==consName) {
                var stuName = sheetLog.getRange(k, colStuName).getValue();
                var stuEmail = sheetLog.getRange(k, colStuEmail).getValue();
                MailApp.sendEmail(subEmail+", "+stuEmail, "Notice: the consultant for your appointment on "+thisDateTime+" has changed", "Hi "+stuName+",\n\nThis is just to let you know that "+consName+" has traded their upcoming shift to "+subName+", who will now be the consultant for your appointment on "+thisDateTime+".");

                var eventId = sheetLog.getRange(k, colEvent).getValue();
                try {
                  CalendarApp.getEventById(eventId).addGuest(subEmail).removeGuest(consEmail);
                } catch {}

                sheetLog.getRange(k, colConsName).setValue(subName);
                sheetLog.getRange(k, colConsName).setValue(subName);
              }
            }
          }
          
          cell.clear();
        }
      }
    }
  }
}

// this function is triggered every 5 minutes.
// logs all new appointments; checks for cancellations and substitutions
function updateLog() {
  logInPersonAppointments();
  
  logRemoteAppointments(rangeThisWeekRemote);
  logRemoteAppointments(rangeNextWeekRemote);

  checkForCancels();
  checkForSubRequests();
  checkForSubConfirms();
}

// saves a copy of the spreadsheet in its current state to a folder
function backUpLog() {
  var date = new Date();
  var formattedDate = formatDate(date, includeTime=true);
  var backupSS = ss.copy("Log Backup "+formattedDate);
  DriveApp.getFileById(backupSS.getId()).moveTo(folderBackups);
}

// this function is triggered every Sunday.
// if the current day is the first day of next week's schedule, backs up log; copies next week's schedule to this week's schedule;
// creates next week's schedule; fills the remote shift section with appointment signup forms
function refreshSignupSheet() { 
  var date = rangeNextWeekRemote.getCell(1, 2).getValue();
  var today = new Date();

  if (formatDate(date)==formatDate(today)) {
    try {
      backUpLog();

      rangeNextWeekRemote.copyTo(rangeThisWeekRemote);
      rangeThisWeekRemote.getCell(1, 1).setValue("This Week");
      rangeNextWeekInPerson.copyTo(rangeThisWeekInPerson);
      rangeThisWeekInPerson.getCell(1, 1).setValue("This Week");
      rangeStaticRemote.copyTo(rangeNextWeekRemote);
      rangeNextWeekRemote.getCell(1, 1).setValue("Next Week");
      rangeStaticInPerson.copyTo(rangeNextWeekInPerson);
      rangeNextWeekInPerson.getCell(1, 1).setValue("Next Week");

      var numCols = rangeNextWeekRemote.getNumColumns();
      var numRows = rangeNextWeekRemote.getNumRows();

      for (var i=2; i<=numCols; ++i) {
        var newDate = new Date(date.getTime()+(5+i)*MILLIS_PER_DAY);
        var formattedDate = formatDate(newDate);
        rangeNextWeekRemote.getCell(1, i).setValue(formattedDate);
        rangeNextWeekInPerson.getCell(1, i).setValue(formattedDate);

        for (var j=2; j<=numRows; ++j) {
          createRemoteSignUpForm(rangeNextWeekRemote.getCell(j, i));
        }
      }
    } catch(err) {
      MailApp.sendEmail("w******@kenyon.edu","Log Backup Failed",err)
    }
  }
}
