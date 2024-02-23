case class MeetingTime(startHour: Int, endHour: Int)

def createMeetingApiCall(
    names: List[String],
    meetingTime: MeetingTime
): Unit = ???
def calendarEntriesApiCall(name: String): List[MeetingTime] = ???

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.effect.unsafe.implicits._
import cats.syntax.traverse._

def calenderEnties(name: String): IO[List[MeetingTime]] = {
  IO.delay(calendarEntriesApiCall(name))
}
def createMeeting(
    names: List[String],
    meetingTime: MeetingTime
): IO[Unit] = {
  IO.delay(createMeetingApiCall(names, meetingTime))
}

def scheduledMeetings(
    attendees: List[String]
): IO[List[MeetingTime]] = {
  attendees
    .map(attendee =>
      retry(calenderEnties(attendee), 10)
    ) // List[IO[List[MeetingTime]]]
    .sequence // IO[List[List[MeetingTime]]]
    .map(_.flatten)
}

// 被っていたらfalse
def meetingNotOverlap(meeting1: MeetingTime, meeting2: MeetingTime): Boolean = {
  meeting1.startHour > meeting2.endHour || meeting1.endHour < meeting2.startHour
}

def possibleMeetings(
    existingMeetings: List[MeetingTime],
    startHour: Int,
    endHour: Int,
    lengthHour: Int
): List[MeetingTime] = {
  val slots = List
    .range(startHour, endHour - lengthHour + 1)
    .map(s => MeetingTime(s, s + lengthHour))
  slots.filter(slot =>
    existingMeetings.forall(meeting => meetingNotOverlap(meeting, slot))
  )
}

def schedule(
    person1: String,
    person2: String,
    lengthHour: Int
): IO[Option[MeetingTime]] = {
  for {
    // IOのリカバリーをどこにするかはビジネス要件によって変わってくるが、簡潔にリカバリーできるのはメリット
    existingMeetings <- retry(
      scheduledMeetings(List(person1, person2)),
      1
    )
      .orElse(IO.pure(List.empty))
    meetings = possibleMeetings(existingMeetings, 8, 16, lengthHour)
    possibleMeeting = meetings.headOption
    _ <- possibleMeeting match {
      case Some(meeting) => createMeeting(List(person1, person2), meeting)
      case None          => IO.unit
    }
  } yield possibleMeeting
}

def retry[A](action: IO[A], maxRetries: Int): IO[A] = {
  // val actions = List.range(0, maxRetries).map(_ => action) // List(action, action, ...)
  // actions.foldLeft(action)((program, nextAction) => program.orElse(nextAction)) // action.orElse(action).orElse(action)
  List
    .range(0, maxRetries)
    .map(_ => action)
    .foldLeft(action)((program, nextAction) => program.orElse(nextAction))
}
