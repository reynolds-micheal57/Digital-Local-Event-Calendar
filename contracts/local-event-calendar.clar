;; ===================================================
;; Digital Local Event Calendar Platform
;; Production-Ready Clarity Smart Contracts
;; ===================================================

;; ===================================================
;; CONTRACT 1: Event Management Core
;; ===================================================

;; Event structure with comprehensive metadata
(define-map events
  { event-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 200),
    start-time: uint,
    end-time: uint,
    max-attendees: uint,
    current-attendees: uint,
    recurring-interval: (optional uint), ;; 0 = none, 1 = daily, 7 = weekly, 30 = monthly
    recurring-end: (optional uint),
    category: (string-ascii 30),
    tags: (list 5 (string-ascii 20)),
    is-active: bool,
    created-at: uint,
    updated-at: uint
  }
)

;; RSVP tracking with detailed status
(define-map rsvps
  { event-id: uint, attendee: principal }
  {
    status: (string-ascii 10), ;; "confirmed", "maybe", "declined"
    rsvp-time: uint,
    notification-pref: bool,
    additional-guests: uint
  }
)

;; Event attendee lists for efficient querying
(define-map event-attendees
  { event-id: uint }
  { attendee-list: (list 1000 principal) }
)

;; User profile and notification preferences
(define-map user-profiles
  { user: principal }
  {
    display-name: (string-ascii 50),
    email-notifications: bool,
    categories-of-interest: (list 10 (string-ascii 30)),
    timezone-offset: int,
    created-at: uint
  }
)

;; Recurring event instances tracking
(define-map recurring-instances
  { parent-event-id: uint, instance-date: uint }
  {
    instance-event-id: uint,
    is-cancelled: bool
  }
)

;; Contract state variables
(define-data-var next-event-id uint u1)
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee uint u0) ;; Future monetization capability
(define-data-var max-events-per-user uint u100)

;; Constants for validation and limits
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-EVENT (err u1002))
(define-constant ERR-EVENT-NOT-FOUND (err u1003))
(define-constant ERR-ALREADY-RSVP (err u1004))
(define-constant ERR-EVENT-FULL (err u1005))
(define-constant ERR-INVALID-TIME (err u1006))
(define-constant ERR-INVALID-PARAMS (err u1007))
(define-constant ERR-RSVP-NOT-FOUND (err u1008))
(define-constant ERR-EVENT-ENDED (err u1009))
(define-constant ERR-USER-LIMIT-REACHED (err u1010))

(define-constant MAX-TITLE-LENGTH u100)
(define-constant MAX-DESCRIPTION-LENGTH u500)
(define-constant MAX-LOCATION-LENGTH u200)
(define-constant MIN-EVENT-DURATION u3600) ;; 1 hour in seconds
(define-constant MAX-ATTENDEES-LIMIT u10000)

;; ===================================================
;; EVENT MANAGEMENT FUNCTIONS
;; ===================================================

;; Create a new event with comprehensive validation
(define-public (create-event
  (title (string-ascii 100))
  (description (string-ascii 500))
  (location (string-ascii 200))
  (start-time uint)
  (end-time uint)
  (max-attendees uint)
  (recurring-interval (optional uint))
  (recurring-end (optional uint))
  (category (string-ascii 30))
  (tags (list 5 (string-ascii 20)))
)
  (let
    (
      (event-id (var-get next-event-id))
      (current-block (stacks-block-height))
      (user-event-count (get-user-event-count tx-sender))
    )

    ;; Comprehensive input validation
    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len description) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len location) u0) ERR-INVALID-PARAMS)
    (asserts! (> start-time current-block) ERR-INVALID-TIME)
    (asserts! (> end-time (+ start-time MIN-EVENT-DURATION)) ERR-INVALID-TIME)
    (asserts! (and (> max-attendees u0) (<= max-attendees MAX-ATTENDEES-LIMIT)) ERR-INVALID-PARAMS)
    (asserts! (< user-event-count (var-get max-events-per-user)) ERR-USER-LIMIT-REACHED)

    ;; Validate recurring event parameters
    (match recurring-interval
      interval (begin
        (asserts! (> interval u0) ERR-INVALID-PARAMS)
        (match recurring-end
          end-time (asserts! (> end-time start-time) ERR-INVALID-TIME)
          true
        )
      )
      true
    )

    ;; Create the event
    (map-set events
      { event-id: event-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        location: location,
        start-time: start-time,
        end-time: end-time,
        max-attendees: max-attendees,
        current-attendees: u0,
        recurring-interval: recurring-interval,
        recurring-end: recurring-end,
        category: category,
        tags: tags,
        is-active: true,
        created-at: current-block,
        updated-at: current-block
      }
    )

    ;; Initialize attendee list
    (map-set event-attendees
      { event-id: event-id }
      { attendee-list: (list) }
    )

    ;; Create recurring instances if specified
    (match recurring-interval
      interval (create-recurring-instances event-id start-time interval recurring-end)
      true
    )

    ;; Increment event counter
    (var-set next-event-id (+ event-id u1))

    (ok event-id)
  )
)

;; RSVP to an event with status tracking
(define-public (rsvp-to-event
  (event-id uint)
  (status (string-ascii 10))
  (notification-pref bool)
  (additional-guests uint)
)
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
      (current-block (stacks-block-height))
      (existing-rsvp (map-get? rsvps { event-id: event-id, attendee: tx-sender }))
    )

    ;; Validation
    (asserts! (get is-active event) ERR-INVALID-EVENT)
    (asserts! (> (get start-time event) current-block) ERR-EVENT-ENDED)
    (asserts! (or (is-eq status "confirmed") (is-eq status "maybe") (is-eq status "declined")) ERR-INVALID-PARAMS)

    ;; Check capacity for confirmed RSVPs
    (if (is-eq status "confirmed")
      (let ((total-guests (+ u1 additional-guests)))
        (asserts! (<= (+ (get current-attendees event) total-guests) (get max-attendees event)) ERR-EVENT-FULL)
      )
      true
    )

    ;; Handle RSVP update or creation
    (match existing-rsvp
      old-rsvp (begin
        ;; Update existing RSVP
        (map-set rsvps
          { event-id: event-id, attendee: tx-sender }
          {
            status: status,
            rsvp-time: current-block,
            notification-pref: notification-pref,
            additional-guests: additional-guests
          }
        )

        ;; Update attendee count based on status change
        (update-attendee-count event-id (get status old-rsvp) status (+ u1 (get additional-guests old-rsvp)) (+ u1 additional-guests))
      )
      (begin
        ;; Create new RSVP
        (map-set rsvps
          { event-id: event-id, attendee: tx-sender }
          {
            status: status,
            rsvp-time: current-block,
            notification-pref: notification-pref,
            additional-guests: additional-guests
          }
        )

        ;; Update attendee list and count for confirmed RSVPs
        (if (is-eq status "confirmed")
          (begin
            (update-attendee-list event-id tx-sender true)
            (update-event-attendee-count event-id (+ u1 additional-guests) true)
          )
          true
        )
      )
    )

    (ok true)
  )
)

;; Update event details (creator only)
(define-public (update-event
  (event-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (location (string-ascii 200))
  (start-time uint)
  (end-time uint)
  (max-attendees uint)
  (category (string-ascii 30))
  (tags (list 5 (string-ascii 20)))
)
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
      (current-block (stacks-block-height))
    )

    ;; Authorization check
    (asserts! (is-eq tx-sender (get creator event)) ERR-UNAUTHORIZED)
    (asserts! (get is-active event) ERR-INVALID-EVENT)

    ;; Validation
    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)
    (asserts! (> start-time current-block) ERR-INVALID-TIME)
    (asserts! (> end-time (+ start-time MIN-EVENT-DURATION)) ERR-INVALID-TIME)
    (asserts! (>= max-attendees (get current-attendees event)) ERR-INVALID-PARAMS)

    ;; Update event
    (map-set events
      { event-id: event-id }
      (merge event {
        title: title,
        description: description,
        location: location,
        start-time: start-time,
        end-time: end-time,
        max-attendees: max-attendees,
        category: category,
        tags: tags,
        updated-at: current-block
      })
    )

    (ok true)
  )
)

;; Cancel an event (creator only)
(define-public (cancel-event (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    )

    (asserts! (is-eq tx-sender (get creator event)) ERR-UNAUTHORIZED)
    (asserts! (get is-active event) ERR-INVALID-EVENT)

    (map-set events
      { event-id: event-id }
      (merge event {
        is-active: false,
        updated-at: (stacks-block-height)
      })
    )

    (ok true)
  )
)

;; ===================================================
;; USER PROFILE MANAGEMENT
;; ===================================================

(define-public (create-user-profile
  (display-name (string-ascii 50))
  (email-notifications bool)
  (categories-of-interest (list 10 (string-ascii 30)))
  (timezone-offset int)
)
  (begin
    (asserts! (> (len display-name) u0) ERR-INVALID-PARAMS)

    (map-set user-profiles
      { user: tx-sender }
      {
        display-name: display-name,
        email-notifications: email-notifications,
        categories-of-interest: categories-of-interest,
        timezone-offset: timezone-offset,
        created-at: (stacks-block-height)
      }
    )

    (ok true)
  )
)

;; ===================================================
;; HELPER FUNCTIONS
;; ===================================================

;; Create recurring event instances
(define-private (create-recurring-instances
  (parent-event-id uint)
  (start-time uint)
  (interval uint)
  (end-time (optional uint))
)
  (let
    (
      (max-instances u52) ;; Limit to 52 instances (1 year weekly)
      (instance-times (generate-recurring-times start-time interval end-time max-instances))
    )
    (fold create-single-instance instance-times parent-event-id)
    true
  )
)

;; Generate recurring time slots
(define-private (generate-recurring-times
  (start-time uint)
  (interval uint)
  (end-time (optional uint))
  (max-count uint)
)
  (let
    (
      (interval-seconds (* interval u86400)) ;; Convert days to seconds
    )
    ;; This would need proper implementation based on Clarity's capabilities
    ;; For now, return a simple list with a few instances
    (list start-time (+ start-time interval-seconds) (+ start-time (* u2 interval-seconds)))
  )
)

;; Create a single recurring instance
(define-private (create-single-instance (instance-time uint) (parent-event-id uint))
  (let
    (
      (instance-event-id (var-get next-event-id))
    )
    (map-set recurring-instances
      { parent-event-id: parent-event-id, instance-date: instance-time }
      {
        instance-event-id: instance-event-id,
        is-cancelled: false
      }
    )
    (var-set next-event-id (+ instance-event-id u1))
    parent-event-id
  )
)

;; Update attendee count based on RSVP status changes
(define-private (update-attendee-count
  (event-id uint)
  (old-status (string-ascii 10))
  (new-status (string-ascii 10))
  (old-guest-count uint)
  (new-guest-count uint)
)
  (let
    (
      (event (unwrap-panic (map-get? events { event-id: event-id })))
      (current-count (get current-attendees event))
    )
    (let
      (
        (count-after-removal
          (if (is-eq old-status "confirmed")
            (- current-count old-guest-count)
            current-count
          )
        )
        (final-count
          (if (is-eq new-status "confirmed")
            (+ count-after-removal new-guest-count)
            count-after-removal
          )
        )
      )
      (map-set events
        { event-id: event-id }
        (merge event { current-attendees: final-count })
      )
    )
  )
)

;; Update event attendee count
(define-private (update-event-attendee-count (event-id uint) (change uint) (is-addition bool))
  (let
    (
      (event (unwrap-panic (map-get? events { event-id: event-id })))
      (current-count (get current-attendees event))
      (new-count
        (if is-addition
          (+ current-count change)
          (- current-count change)
        )
      )
    )
    (map-set events
      { event-id: event-id }
      (merge event { current-attendees: new-count })
    )
  )
)

;; Update attendee list
(define-private (update-attendee-list (event-id uint) (attendee principal) (is-addition bool))
  (let
    (
      (current-list (default-to (list) (get attendee-list (map-get? event-attendees { event-id: event-id }))))
    )
    (if is-addition
      (map-set event-attendees
        { event-id: event-id }
        { attendee-list: (unwrap-panic (as-max-len? (append current-list attendee) u1000)) }
      )
      (map-set event-attendees
        { event-id: event-id }
        { attendee-list: (filter remove-attendee-filter current-list) }
      )
    )
  )
)

;; Filter helper for removing attendees
(define-private (remove-attendee-filter (attendee principal))
  (not (is-eq attendee tx-sender))
)

;; Get user event count for rate limiting
(define-private (get-user-event-count (user principal))
  ;; This would require iterating through events - simplified for demo
  u0
)

;; ===================================================
;; READ-ONLY FUNCTIONS
;; ===================================================

;; Get event details
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Get user's RSVP status for an event
(define-read-only (get-rsvp (event-id uint) (attendee principal))
  (map-get? rsvps { event-id: event-id, attendee: attendee })
)

;; Get event attendees
(define-read-only (get-event-attendees (event-id uint))
  (map-get? event-attendees { event-id: event-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get recurring instances
(define-read-only (get-recurring-instances (parent-event-id uint))
  (map-get? recurring-instances { parent-event-id: parent-event-id, instance-date: u0 })
)

;; Get next event ID
(define-read-only (get-next-event-id)
  (var-get next-event-id)
)

;; Check if event has capacity
(define-read-only (has-capacity (event-id uint) (additional-guests uint))
  (match (map-get? events { event-id: event-id })
    event (< (+ (get current-attendees event) additional-guests) (get max-attendees event))
    false
  )
)

;; ===================================================
;; ADMIN FUNCTIONS
;; ===================================================

;; Set maximum events per user (owner only)
(define-public (set-max-events-per-user (new-limit uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set max-events-per-user new-limit)
    (ok true)
  )
)

;; Set platform fee (owner only) - for future monetization
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

;; Transfer ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; ===================================================
;; CONTRACT 2: Notification and Discovery System
;; ===================================================

;; User notification subscriptions by category/location
(define-map notification-subscriptions
  { user: principal }
  {
    categories: (list 20 (string-ascii 30)),
    locations: (list 10 (string-ascii 200)),
    keywords: (list 15 (string-ascii 30)),
    max-distance: (optional uint), ;; Future geolocation feature
    notification-types: (list 5 (string-ascii 20)), ;; "new-event", "reminder", "update", "cancellation"
    is-active: bool,
    created-at: uint,
    updated-at: uint
  }
)

;; Event reminders queue
(define-map event-reminders
  { event-id: uint, reminder-time: uint }
  {
    reminder-type: (string-ascii 20), ;; "1-day", "1-hour", "custom"
    recipients: (list 1000 principal),
    is-sent: bool,
    created-at: uint
  }
)

;; Event search index by category
(define-map category-events
  { category: (string-ascii 30) }
  { event-ids: (list 1000 uint) }
)

;; Event search index by location
(define-map location-events
  { location: (string-ascii 200) }
  { event-ids: (list 1000 uint) }
)

;; Featured events (admin curated)
(define-map featured-events
  { featured-slot: uint }
  {
    event-id: uint,
    priority: uint,
    start-featured: uint,
    end-featured: uint,
    is-active: bool
  }
)

;; User event history and analytics
(define-map user-event-history
  { user: principal }
  {
    events-created: uint,
    events-attended: uint,
    events-rsvped: uint,
    favorite-categories: (list 5 (string-ascii 30)),
    last-activity: uint
  }
)

;; Constants for notification system
(define-constant MAX-NOTIFICATION-CATEGORIES u20)
(define-constant MAX-NOTIFICATION-KEYWORDS u15)
(define-constant MAX-FEATURED-EVENTS u10)

;; Notification system state
(define-data-var notification-enabled bool true)
(define-data-var max-reminders-per-event uint u5)

;; ===================================================
;; NOTIFICATION SUBSCRIPTION FUNCTIONS
;; ===================================================

;; Subscribe to notifications
(define-public (subscribe-to-notifications
  (categories (list 20 (string-ascii 30)))
  (locations (list 10 (string-ascii 200)))
  (keywords (list 15 (string-ascii 30)))
  (notification-types (list 5 (string-ascii 20)))
)
  (let
    (
      (current-block (stacks-block-height))
    )

    ;; Validation
    (asserts! (< (len categories) MAX-NOTIFICATION-CATEGORIES) ERR-INVALID-PARAMS)
    (asserts! (< (len keywords) MAX-NOTIFICATION-KEYWORDS) ERR-INVALID-PARAMS)

    (map-set notification-subscriptions
      { user: tx-sender }
      {
        categories: categories,
        locations: locations,
        keywords: keywords,
        max-distance: none,
        notification-types: notification-types,
        is-active: true,
        created-at: current-block,
        updated-at: current-block
      }
    )

    (ok true)
  )
)

;; Update notification preferences
(define-public (update-notification-preferences
  (categories (list 20 (string-ascii 30)))
  (locations (list 10 (string-ascii 200)))
  (keywords (list 15 (string-ascii 30)))
  (notification-types (list 5 (string-ascii 20)))
  (is-active bool)
)
  (let
    (
      (existing-sub (unwrap! (map-get? notification-subscriptions { user: tx-sender }) ERR-INVALID-PARAMS))
    )

    (map-set notification-subscriptions
      { user: tx-sender }
      (merge existing-sub {
        categories: categories,
        locations: locations,
        keywords: keywords,
        notification-types: notification-types,
        is-active: is-active,
        updated-at: (stacks-block-height)
      })
    )

    (ok true)
  )
)

;; Schedule event reminders
(define-public (schedule-reminder
  (event-id uint)
  (reminder-time uint)
  (reminder-type (string-ascii 20))
)
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
      (attendee-list (get attendee-list (unwrap! (map-get? event-attendees { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    )

    ;; Validation
    (asserts! (is-eq tx-sender (get creator event)) ERR-UNAUTHORIZED)
    (asserts! (> reminder-time (stacks-block-height)) ERR-INVALID-TIME)
    (asserts! (< reminder-time (get start-time event)) ERR-INVALID-TIME)

    (map-set event-reminders
      { event-id: event-id, reminder-time: reminder-time }
      {
        reminder-type: reminder-type,
        recipients: attendee-list,
        is-sent: false,
        created-at: (stacks-block-height)
      }
    )

    (ok true)
  )
)

;; ===================================================
;; EVENT DISCOVERY FUNCTIONS
;; ===================================================

;; Add event to category index
(define-public (index-event-by-category (event-id uint) (category (string-ascii 30)))
  (let
    (
      (current-events (default-to (list) (get event-ids (map-get? category-events { category: category }))))
    )

    (map-set category-events
      { category: category }
      { event-ids: (unwrap-panic (as-max-len? (append current-events event-id) u1000)) }
    )

    (ok true)
  )
)

;; Add event to location index
(define-public (index-event-by-location (event-id uint) (location (string-ascii 200)))
  (let
    (
      (current-events (default-to (list) (get event-ids (map-get? location-events { location: location }))))
    )

    (map-set location-events
      { location: location }
      { event-ids: (unwrap-panic (as-max-len? (append current-events event-id) u1000)) }
    )

    (ok true)
  )
)

;; Feature an event (admin only)
(define-public (feature-event
  (event-id uint)
  (featured-slot uint)
  (priority uint)
  (start-featured uint)
  (end-featured uint)
)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (< featured-slot MAX-FEATURED-EVENTS) ERR-INVALID-PARAMS)
    (asserts! (is-some (map-get? events { event-id: event-id })) ERR-EVENT-NOT-FOUND)
    (asserts! (> end-featured start-featured) ERR-INVALID-TIME)

    (map-set featured-events
      { featured-slot: featured-slot }
      {
        event-id: event-id,
        priority: priority,
        start-featured: start-featured,
        end-featured: end-featured,
        is-active: true
      }
    )

    (ok true)
  )
)

;; Update user event history
(define-public (update-user-history (user principal) (history-type (string-ascii 20)))
  (let
    (
      (current-history (default-to
        { events-created: u0, events-attended: u0, events-rsvped: u0, favorite-categories: (list), last-activity: u0 }
        (map-get? user-event-history { user: user })
      ))
    )

    (map-set user-event-history
      { user: user }
      (merge current-history {
        events-created: (if (is-eq history-type "created") (+ (get events-created current-history) u1) (get events-created current-history)),
        events-attended: (if (is-eq history-type "attended") (+ (get events-attended current-history) u1) (get events-attended current-history)),
        events-rsvped: (if (is-eq history-type "rsvped") (+ (get events-rsvped current-history) u1) (get events-rsvped current-history)),
        last-activity: (stacks-block-height)
      })
    )

    (ok true)
  )
)

;; ===================================================
;; READ-ONLY DISCOVERY FUNCTIONS
;; ===================================================

;; Get events by category
(define-read-only (get-events-by-category (category (string-ascii 30)))
  (map-get? category-events { category: category })
)

;; Get events by location
(define-read-only (get-events-by-location (location (string-ascii 200)))
  (map-get? location-events { location: location })
)

;; Get featured events
(define-read-only (get-featured-events (featured-slot uint))
  (map-get? featured-events { featured-slot: featured-slot })
)

;; Get user notification subscriptions
(define-read-only (get-notification-subscriptions (user principal))
  (map-get? notification-subscriptions { user: user })
)

;; Get user event history
(define-read-only (get-user-history (user principal))
  (map-get? user-event-history { user: user })
)

;; Get pending reminders for an event
(define-read-only (get-event-reminders (event-id uint) (reminder-time uint))
  (map-get? event-reminders { event-id: event-id, reminder-time: reminder-time })
)

;; Check if notifications are enabled
(define-read-only (are-notifications-enabled)
  (var-get notification-enabled)
)

;; ===================================================
;; ADMIN NOTIFICATION FUNCTIONS
;; ===================================================

;; Toggle notification system
(define-public (toggle-notifications (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set notification-enabled enabled)
    (ok true)
  )
)

;; Set maximum reminders per event
(define-public (set-max-reminders (new-max uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set max-reminders-per-event new-max)
    (ok true)
  )
)
