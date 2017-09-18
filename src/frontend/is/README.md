# Frontend for communication with IS MU questinares

*   FastCGI, has to be plugged into a web server to work, listens on unix socket
*   input format: post request parameters:
        *   `id` - ID of the question, contains, of form `AUTH:COURSE:QID`
            *   `AUTH` - either `[ANYTHING]` as an identifier of unauthorized
                service, or 16 chars Base64 token of authorized service
            *   `COURSE` - IS identifier of the course
            *   `QID` - question ID, has to be unique in the course
        *   `odp` - the student's answer
        *   `uco` - student's ID number, will not be present for teacher
            listings
*   output format is just plain text: `POINTS~~TEXTUAL_ASSESMENT`, `POINTS` can
    be either `ok` or `nok`
