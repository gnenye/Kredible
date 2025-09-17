;; Decentralized Reputation System
;; Allows users to build, earn, and manage reputation across different domains
;; and applications while maintaining privacy and control over their reputation data

(define-map credibility-sectors
  { sector-id: uint }
  {
    label: (string-utf8 64),
    summary: (string-utf8 256),
    manager: principal,
    established-at: uint,
    operational: bool,
    recommendation-weight: uint,       ;; Weight of endorsements (out of 100)
    engagement-weight: uint,          ;; Weight of activity (out of 100)
    validation-weight: uint,      ;; Weight of verifications (out of 100)
    min-recommendations-required: uint ;; Minimum endorsements needed for reputation score
  }
)

(define-map participant-credibility
  { sector-id: uint, participant: principal }
  {
    credibility-score: uint,         ;; 0-1000 score
    recommendation-count: uint,        ;; Number of endorsements received
    engagement-count: uint,           ;; Number of recorded activities
    validation-level: uint,       ;; 0-5 verification level
    total-composite-score: uint,     ;; Sum of weighted component scores
    last-modified: uint,             ;; Block height of last update
    deterioration-rate: uint                ;; Rate at which reputation decays if inactive (per 1000 blocks)
  }
)

(define-map recommendations
  { sector-id: uint, recommender: principal, recommended: principal }
  {
    strength: uint,                   ;; 1-10 weight of endorsement
    recorded-at: uint,                ;; When endorsement was given
    note: (optional (string-utf8 140)),  ;; Optional message
    skill-areas: (list 5 (string-ascii 20)), ;; Categories being endorsed
    enabled: bool                    ;; Whether endorsement is active
  }
)

(define-map validations
  { sector-id: uint, participant: principal, validation-category: (string-ascii 32) }
  {
    validated-by: principal,         ;; Who performed verification
    validated-when: uint,              ;; When verification was done
    expires-when: (optional uint),    ;; When verification expires
    proof-hash: (buff 32),       ;; Hash of verification evidence
    tier: uint,                    ;; 1-5 verification level
    enabled: bool                    ;; Whether verification is active
  }
)

(define-map credibility-engagements
  { sector-id: uint, engagement-id: uint }
  {
    participant: principal,                ;; User who performed activity
    engagement-category: (string-ascii 32), ;; Type of activity
    recorded-at: uint,                ;; When activity was recorded
    magnitude: uint,                    ;; Value of activity (domain-specific)
    content-hash: (buff 32),           ;; Hash of activity data
    confirmed: bool,                 ;; Whether activity is verified
    confirmed-by: (optional principal) ;; Who verified the activity
  }
)

(define-map authorized-validation-services
  { sector-id: uint, service: principal }
  {
    service-name: (string-utf8 64),
    approved-by: principal,
    approved-when: uint,
    validation-categories: (list 10 (string-ascii 32)),
    operational: bool
  }
)

(define-map credibility-delegations
  { sector-id: uint, delegator: principal }
  {
    representative: principal,
    delegated-when: uint,
    expires-when: (optional uint),
    operational: bool
  }
)

(define-map visibility-preferences
  { sector-id: uint, participant: principal }
  {
    visible-score: bool,             ;; Whether score is publicly viewable
    visible-recommendations: bool,      ;; Whether endorsements are publicly viewable
    visible-engagements: bool,        ;; Whether activities are publicly viewable
    visible-validations: bool,     ;; Whether verifications are publicly viewable
    permitted-viewers: (list 10 principal) ;; Principals authorized to view private data
  }
)

;; Next available IDs
(define-data-var next-sector-id uint u0)
(define-map next-engagement-id { sector-id: uint } { id: uint })

;; Define our own min function since it's not built-in
(define-private (get-minimum (x uint) (y uint))
  (if (<= x y) x y)
)

;; Define our own max function since we'll need it
(define-private (get-maximum (x uint) (y uint))
  (if (>= x y) x y)
)

;; Validate domain ID
(define-private (validate-sector-id (sector-id uint))
  (if (< sector-id (var-get next-sector-id))
      (ok sector-id)
      (err u"Invalid sector ID"))
)

;; Validate string-utf8-64
(define-private (validate-utf8-text-64 (val (string-utf8 64)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate string-utf8-256
(define-private (validate-utf8-text-256 (val (string-utf8 256)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate string-ascii-32
(define-private (validate-ascii-text-32 (val (string-ascii 32)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate weight (1-10)
(define-private (validate-strength (strength uint))
  (if (and (>= strength u1) (<= strength u10))
      (ok strength)
      (err u"Strength must be between 1 and 10"))
)

;; Validate verification level (1-5)
(define-private (validate-validation-tier (tier uint))
  (if (and (>= tier u1) (<= tier u5))
      (ok tier)
      (err u"Tier must be between 1 and 5"))
)

;; Check if user can view another user's private data
(define-private (can-access-private-data (sector-id uint) (owner principal) (viewer principal))
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (asserts! (is-ok validated-sector-id-resp) false)
    
    (if (is-eq owner viewer)
        ;; Owner can always view own data
        true
        ;; Check privacy settings
        (match (map-get? visibility-preferences { sector-id: sector-id, participant: owner })
          settings (is-some (index-of? (get permitted-viewers settings) viewer))
          false
        )
    )
  )
)

;; Create a new reputation domain
(define-public (create-credibility-sector
                (label (string-utf8 64))
                (summary (string-utf8 256))
                (recommendation-weight uint)
                (engagement-weight uint)
                (validation-weight uint)
                (min-recommendations-required uint))
  (let
    ((sector-id (var-get next-sector-id))
     (validated-label-resp (validate-utf8-text-64 label))
     (validated-summary-resp (validate-utf8-text-256 summary)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-label-resp) 
              (err (unwrap-err! validated-label-resp (err u"Invalid label"))))
    (asserts! (is-ok validated-summary-resp) 
              (err (unwrap-err! validated-summary-resp (err u"Invalid summary"))))
    (asserts! (< (+ (+ recommendation-weight engagement-weight) validation-weight) u101) 
              (err u"Weights must sum to 100 or less"))
    (asserts! (> min-recommendations-required u0) (err u"Minimum recommendations must be greater than 0"))
    
    ;; Create the domain
    (map-set credibility-sectors
      { sector-id: sector-id }
      {
        label: (unwrap-panic validated-label-resp),
        summary: (unwrap-panic validated-summary-resp),
        manager: tx-sender,
        established-at: block-height,
        operational: true,
        recommendation-weight: recommendation-weight,
        engagement-weight: engagement-weight,
        validation-weight: validation-weight,
        min-recommendations-required: min-recommendations-required
      }
    )
    
    ;; Initialize activity counter
    (map-set next-engagement-id
      { sector-id: sector-id }
      { id: u0 }
    )
    
    ;; Increment domain ID counter
    (var-set next-sector-id (+ sector-id u1))
    
    (ok sector-id)
  )
)

;; Endorse a user
(define-public (recommend-participant
                (sector-id uint)
                (recommended principal)
                (strength uint)
                (note (optional (string-utf8 140)))
                (skill-areas (list 5 (string-ascii 20))))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id))
     (validated-strength-resp (validate-strength strength)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    (asserts! (is-ok validated-strength-resp) 
              (err (unwrap-err! validated-strength-resp (err u"Invalid strength"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (validated-strength (unwrap-panic validated-strength-resp))
          (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                         (err u"Sector not found")))
          (existing-recommendation (map-get? recommendations 
                                { sector-id: validated-sector-id, recommender: tx-sender, recommended: recommended })))
      
      ;; Validate
      (asserts! (not (is-eq tx-sender recommended)) (err u"Cannot recommend yourself"))
      (asserts! (get operational sector) (err u"Sector not operational"))
      
      ;; Create or update the endorsement
      (map-set recommendations
        { sector-id: validated-sector-id, recommender: tx-sender, recommended: recommended }
        {
          strength: validated-strength,
          recorded-at: block-height,
          note: note,
          skill-areas: skill-areas,
          enabled: true
        }
      )
      
      ;; Update endorsee's reputation if no previous endorsement
      (if (is-none existing-recommendation)
          (update-recommendation-count validated-sector-id recommended u1)
          true)
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-credibility-score validated-sector-id recommended)))
        (ok score))
    )
  )
)

;; Private helper to update endorsement count
(define-private (update-recommendation-count (sector-id uint) (participant principal) (delta uint))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    (asserts! (is-ok validated-sector-id-resp) false)
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (credibility (default-to  
                       {
                         credibility-score: u0,
                         recommendation-count: u0,
                         engagement-count: u0,
                         validation-level: u0,
                         total-composite-score: u0,
                         last-modified: block-height,
                         deterioration-rate: u10  ;; Default 1% decay per 1000 blocks
                       }
                       (map-get? participant-credibility { sector-id: validated-sector-id, participant: participant }))))
      
      (map-set participant-credibility
        { sector-id: validated-sector-id, participant: participant }
        (merge credibility { recommendation-count: (+ (get recommendation-count credibility) delta) })
      )
      
      ;; Changed from (ok true) to just true since this is a private function
      true
    )
  )
)

;; Remove an endorsement
(define-public (remove-recommendation (sector-id uint) (recommended principal))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (recommendation (unwrap! (map-get? recommendations 
                               { sector-id: validated-sector-id, recommender: tx-sender, recommended: recommended })
                              (err u"Recommendation not found"))))
      
      ;; Update the endorsement
      (map-set recommendations
        { sector-id: validated-sector-id, recommender: tx-sender, recommended: recommended }
        (merge recommendation { enabled: false })
      )
      
      ;; Update endorsee's reputation (using directly)
      (update-recommendation-count validated-sector-id recommended (- u0 u1))
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-credibility-score validated-sector-id recommended)))
        (ok score))
    )
  )
)

;; Record an activity for reputation
(define-public (record-engagement
                (sector-id uint)
                (engagement-category (string-ascii 32))
                (magnitude uint)
                (content-hash (buff 32)))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id))
     (validated-engagement-category-resp (validate-ascii-text-32 engagement-category)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    (asserts! (is-ok validated-engagement-category-resp) 
              (err (unwrap-err! validated-engagement-category-resp (err u"Invalid engagement category"))))
    (asserts! (> (len content-hash) u0) (err u"Content hash cannot be empty"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (validated-engagement-category (unwrap-panic validated-engagement-category-resp))
          (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                         (err u"Sector not found")))
          (engagement-counter (unwrap! (map-get? next-engagement-id { sector-id: validated-sector-id }) 
                                   (err u"Counter not found")))
          (engagement-id (get id engagement-counter))
          (credibility (default-to  
                       {
                         credibility-score: u0,
                         recommendation-count: u0,
                         engagement-count: u0,
                         validation-level: u0,
                         total-composite-score: u0,
                         last-modified: block-height,
                         deterioration-rate: u10  ;; Default 1% decay per 1000 blocks
                       }
                       (map-get? participant-credibility { sector-id: validated-sector-id, participant: tx-sender }))))
      
      ;; Validate
      (asserts! (get operational sector) (err u"Sector not operational"))
      
      ;; Record the activity
      (map-set credibility-engagements
        { sector-id: validated-sector-id, engagement-id: engagement-id }
        {
          participant: tx-sender,
          engagement-category: validated-engagement-category,
          recorded-at: block-height,
          magnitude: magnitude,
          content-hash: content-hash,
          confirmed: false,
          confirmed-by: none
        }
      )
      
      ;; Update activity counter
      (map-set next-engagement-id
        { sector-id: validated-sector-id }
        { id: (+ engagement-id u1) }
      )
      
      ;; Update user's activity count
      (map-set participant-credibility
        { sector-id: validated-sector-id, participant: tx-sender }
        (merge credibility { 
          engagement-count: (+ (get engagement-count credibility) u1),
          last-modified: block-height
        })
      )
      
      ;; Recalculate reputation score and return the activity ID
      (calculate-credibility-score validated-sector-id tx-sender)
      (ok engagement-id)
    )
  )
)

;; Verify an activity (by domain admin or delegated verifier)
(define-public (verify-engagement
                (sector-id uint)
                (engagement-id uint))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                         (err u"Sector not found")))
          (engagement (unwrap! (map-get? credibility-engagements 
                            { sector-id: validated-sector-id, engagement-id: engagement-id })
                           (err u"Engagement not found"))))
      
      ;; Validate
      (asserts! (or (is-eq tx-sender (get manager sector))
                   (is-authorized-validator validated-sector-id tx-sender))
                (err u"Not authorized to verify"))
      (asserts! (not (get confirmed engagement)) (err u"Engagement already confirmed"))
      
      ;; Update activity
      (map-set credibility-engagements
        { sector-id: validated-sector-id, engagement-id: engagement-id }
        (merge engagement { 
          confirmed: true,
          confirmed-by: (some tx-sender)
        })
      )
      
      ;; Recalculate reputation score for the activity owner and return success
      (let ((score (calculate-credibility-score validated-sector-id (get participant engagement))))
        (ok score))
    )
  )
)

;; Check if principal is a delegated verifier
(define-private (is-authorized-validator (sector-id uint) (service principal))
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (if (is-ok validated-sector-id-resp)
        (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
          (default-to 
            false 
            (get operational (map-get? authorized-validation-services 
                        { sector-id: validated-sector-id, service: service }))
          )
        )
        false
    )
  )
)

;; Add verification for a user
(define-public (add-validation
    (sector-id uint)
    (participant principal)
    (validation-category (string-ascii 32))
    (proof-hash (buff 32))
    (tier uint)
    (expires-when (optional uint)))
    (let ((validated-sector-id-resp (validate-sector-id sector-id))
          (validated-validation-category-resp (validate-ascii-text-32 validation-category))
          (validated-tier-resp (validate-validation-tier tier)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-sector-id-resp) 
                  (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
        (asserts! (is-ok validated-validation-category-resp) 
                  (err (unwrap-err! validated-validation-category-resp (err u"Invalid validation category"))))
        (asserts! (is-ok validated-tier-resp) 
                  (err (unwrap-err! validated-tier-resp (err u"Invalid tier"))))
        (asserts! (> (len proof-hash) u0) (err u"Proof hash cannot be empty"))
        
        (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
              (validated-validation-category (unwrap-panic validated-validation-category-resp))
              (validated-tier (unwrap-panic validated-tier-resp))
              (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                             (err u"Sector not found")))
              (credibility (default-to {
                  credibility-score: u0,
                  recommendation-count: u0,
                  engagement-count: u0,
                  validation-level: u0,
                  total-composite-score: u0,
                  last-modified: block-height,
                  deterioration-rate: u10
              } (map-get? participant-credibility { sector-id: validated-sector-id, participant: participant })))
              (current-validation-tier (get validation-level credibility)))
            
            ;; Validate
            (asserts! (or (is-eq tx-sender (get manager sector))
                         (is-authorized-validator validated-sector-id tx-sender))
                      (err u"Not authorized to validate"))
            
            ;; Add verification
            (map-set validations
                { sector-id: validated-sector-id, participant: participant, validation-category: validated-validation-category }
                {
                    validated-by: tx-sender,
                    validated-when: block-height,
                    expires-when: expires-when,
                    proof-hash: proof-hash,
                    tier: validated-tier,
                    enabled: true
                }
            )
            
            ;; Update user's verification level (take highest verification level)
            (map-set participant-credibility
                { sector-id: validated-sector-id, participant: participant }
                (merge credibility { 
                    validation-level: (get-maximum current-validation-tier validated-tier),
                    last-modified: block-height
                })
            )
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-credibility-score validated-sector-id participant)))
                (ok score))
        )
    )
)

(define-public (revoke-validation
    (sector-id uint)
    (participant principal)
    (validation-category (string-ascii 32)))
    (let ((validated-sector-id-resp (validate-sector-id sector-id))
          (validated-validation-category-resp (validate-ascii-text-32 validation-category)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-sector-id-resp) 
                  (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
        (asserts! (is-ok validated-validation-category-resp) 
                  (err (unwrap-err! validated-validation-category-resp (err u"Invalid validation category"))))
        
        (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
              (validated-validation-category (unwrap-panic validated-validation-category-resp))
              (validation (unwrap! (map-get? validations 
                                    { sector-id: validated-sector-id, participant: participant, validation-category: validated-validation-category })
                                    (err u"Validation not found"))))
            
            ;; Validate
            (asserts! (is-eq tx-sender (get validated-by validation)) 
                      (err u"Only validator can revoke"))
            
            ;; Update verification
            (map-set validations
                { sector-id: validated-sector-id, participant: participant, validation-category: validated-validation-category }
                (merge validation { enabled: false })
            )
            
            ;; Recalculate highest verification level
            (recalculate-validation-tier validated-sector-id participant)
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-credibility-score validated-sector-id participant)))
                (ok score))
        )
    )
)

(define-private (recalculate-validation-tier 
    (sector-id uint) 
    (participant principal))
    (let ((validated-sector-id-resp (validate-sector-id sector-id)))
        (asserts! (is-ok validated-sector-id-resp) false)
        
        (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
              (credibility (default-to {
                  credibility-score: u0,
                  recommendation-count: u0,
                  engagement-count: u0,
                  validation-level: u0,
                  total-composite-score: u0,
                  last-modified: block-height,
                  deterioration-rate: u10
              } (map-get? participant-credibility { sector-id: validated-sector-id, participant: participant }))))
            
            ;; For this example, we'll just reset to level 0
            (map-set participant-credibility
                { sector-id: validated-sector-id, participant: participant }
                (merge credibility { validation-level: u0 })
            )
            
            true
        )
    )
)

;; Calculate reputation score
(define-private (calculate-credibility-score (sector-id uint) (participant principal))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    (asserts! (is-ok validated-sector-id-resp) u0)
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (sector (unwrap-panic (map-get? credibility-sectors { sector-id: validated-sector-id })))
          (credibility (unwrap-panic (map-get? participant-credibility { sector-id: validated-sector-id, participant: participant })))
         
          ;; Calculate component scores
          (recommendation-score (calculate-recommendation-score validated-sector-id participant 
                                                       (get recommendation-count credibility)))
          (engagement-score (calculate-engagement-score validated-sector-id participant 
                                                 (get engagement-count credibility)))
          (validation-score (calculate-validation-score validated-sector-id participant 
                                                         (get validation-level credibility)))
         
          ;; Calculate weighted scores
          (weighted-recommendation (/ (* recommendation-score (get recommendation-weight sector)) u100))
          (weighted-engagement (/ (* engagement-score (get engagement-weight sector)) u100))
          (weighted-validation (/ (* validation-score (get validation-weight sector)) u100))
         
          ;; Calculate total score
          (total-composite (+ (+ weighted-recommendation weighted-engagement) weighted-validation))
          (decayed-score (apply-deterioration total-composite credibility)))
        
        ;; Update reputation score
        (map-set participant-credibility
          { sector-id: validated-sector-id, participant: participant }
          (merge credibility { 
            credibility-score: decayed-score,
            total-composite-score: total-composite,
            last-modified: block-height
          })
        )
        
        ;; Return the score directly, not a response
        decayed-score
    )
  )
)

;; Calculate endorsement score component (0-1000)
(define-private (calculate-recommendation-score (sector-id uint) (participant principal) (recommendation-count uint))
  ;; In a real implementation, this would be a more complex calculation
  ;; For this example, we'll use a simple scaling function
  
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    (asserts! (is-ok validated-sector-id-resp) u0)
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (sector (unwrap-panic (map-get? credibility-sectors { sector-id: validated-sector-id })))
          (min-required (get min-recommendations-required sector)))
        
        (if (< recommendation-count min-required)
            ;; Below minimum: score = (count / min-required) * 500
            (/ (* recommendation-count u500) min-required)
            ;; Above minimum: score = 500 + (count - min-required) * 50, max 1000
            (get-minimum u1000 (+ u500 (* (- recommendation-count min-required) u50)))
        )
    )
  )
)

;; Calculate activity score component (0-1000)
(define-private (calculate-engagement-score (sector-id uint) (participant principal) (engagement-count uint))
  ;; Simplified calculation
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (if (is-ok validated-sector-id-resp)
        (get-minimum u1000 (* engagement-count u100))
        u0
    )
  )
)

;; Calculate verification score component (0-1000)
(define-private (calculate-validation-score (sector-id uint) (participant principal) (validation-level uint))
  ;; Level 0-5 scaled to 0-1000
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (if (is-ok validated-sector-id-resp)
        (* validation-level u200)
        u0
    )
  )
)

;; Apply decay to reputation score based on last update time
(define-private (apply-deterioration (score uint) (credibility (tuple 
                                           (credibility-score uint)
                                           (recommendation-count uint)
                                           (engagement-count uint)
                                           (validation-level uint)
                                           (total-composite-score uint)
                                           (last-modified uint)
                                           (deterioration-rate uint))))
  (let
    ((blocks-since-update (- block-height (get last-modified credibility)))
     (deterioration-periods (/ blocks-since-update u1000))
     (deterioration-rate (get deterioration-rate credibility)))
    
    (if (or (is-eq deterioration-periods u0) (is-eq deterioration-rate u0))
        ;; No decay
        score
        ;; Apply decay: score * (1 - decay-rate/100)^decay-periods
        ;; Simplified calculation
        (- score (/ (* score (* deterioration-periods deterioration-rate)) u1000))
    )
  )
)

;; Add a delegated verification provider
(define-public (add-validation-service
                (sector-id uint)
                (service principal)
                (service-name (string-utf8 64))
                (validation-categories (list 10 (string-ascii 32))))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id))
     (validated-service-name-resp (validate-utf8-text-64 service-name)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    (asserts! (is-ok validated-service-name-resp) 
              (err (unwrap-err! validated-service-name-resp (err u"Invalid service name"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (validated-service-name (unwrap-panic validated-service-name-resp))
          (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                         (err u"Sector not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get manager sector)) (err u"Only sector manager can add services"))
      
      ;; Add provider
      (map-set authorized-validation-services
        { sector-id: validated-sector-id, service: service }
        {
          service-name: validated-service-name,
          approved-by: tx-sender,
          approved-when: block-height,
          validation-categories: validation-categories,
          operational: true
        }
      )
      
      (ok true)
    )
  )
)

;; Revoke a verification provider
(define-public (revoke-validation-service (sector-id uint) (service principal))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (sector (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) 
                         (err u"Sector not found")))
          (service-data (unwrap! (map-get? authorized-validation-services 
                                 { sector-id: validated-sector-id, service: service })
                                (err u"Service not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get manager sector)) (err u"Only sector manager can revoke services"))
      
      ;; Update provider
      (map-set authorized-validation-services
        { sector-id: validated-sector-id, service: service }
        (merge service-data { operational: false })
      )
      
      (ok true)
    )
  )
)

;; Delegate reputation management to another principal
(define-public (delegate-credibility (sector-id uint) (representative principal) (expires-when (optional uint)))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
      (map-set credibility-delegations
        { sector-id: validated-sector-id, delegator: tx-sender }
        {
          representative: representative,
          delegated-when: block-height,
          expires-when: expires-when,
          operational: true
        }
      )
      
      (ok true)
    )
  )
)

;; Remove reputation delegation
(define-public (remove-delegation (sector-id uint))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (delegation (unwrap! (map-get? credibility-delegations 
                              { sector-id: validated-sector-id, delegator: tx-sender })
                             (err u"Delegation not found"))))
      
      (map-set credibility-delegations
        { sector-id: validated-sector-id, delegator: tx-sender }
        (merge delegation { operational: false })
      )
      
      (ok true)
    )
  )
)

;; Update privacy settings
(define-public (update-visibility-preferences
                (sector-id uint)
                (visible-score bool)
                (visible-recommendations bool)
                (visible-engagements bool)
                (visible-validations bool)
                (permitted-viewers (list 10 principal)))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-sector-id-resp) 
              (err (unwrap-err! validated-sector-id-resp (err u"Invalid sector ID"))))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
      (map-set visibility-preferences
        { sector-id: validated-sector-id, participant: tx-sender }
        {
          visible-score: visible-score,
          visible-recommendations: visible-recommendations,
          visible-engagements: visible-engagements,
          visible-validations: visible-validations,
          permitted-viewers: permitted-viewers
        }
      )
      
      (ok true)
    )
  )
)

;; Read-only functions

;; Get domain details
(define-read-only (get-sector-details (sector-id uint))
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
      (ok (unwrap! (map-get? credibility-sectors { sector-id: validated-sector-id }) (err u"Sector not found")))
    )
  )
)

;; Get user reputation score
(define-read-only (get-credibility-score (sector-id uint) (participant principal))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (credibility (map-get? participant-credibility { sector-id: validated-sector-id, participant: participant }))
          (visibility (map-get? visibility-preferences { sector-id: validated-sector-id, participant: participant })))
      
      (if (is-none credibility)
          (err u"Credibility not found")
          (if (or (is-none visibility)
                  (get visible-score (unwrap-panic visibility))
                  (can-access-private-data validated-sector-id participant tx-sender))
              (ok (get credibility-score (unwrap-panic credibility)))
              (err u"Not authorized to view score")
          )
      )
    )
  )
)

;; Get user endorsements
(define-read-only (get-participant-recommendations (sector-id uint) (participant principal))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id)))
    
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (visibility (map-get? visibility-preferences { sector-id: validated-sector-id, participant: participant })))
      
      (if (or (is-none visibility)
              (get visible-recommendations (unwrap-panic visibility))
              (can-access-private-data validated-sector-id participant tx-sender))
          (ok u"Recommendations would be returned here")
          (err u"Not authorized to view recommendations")
      )
    )
  )
)

;; Get verification details
(define-read-only (get-validation (sector-id uint) (participant principal) (validation-category (string-ascii 32)))
  (let
    ((validated-sector-id-resp (validate-sector-id sector-id))
     (validated-validation-category-resp (validate-ascii-text-32 validation-category)))
    
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    (asserts! (is-ok validated-validation-category-resp) (err u"Invalid validation category"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp))
          (validated-validation-category (unwrap-panic validated-validation-category-resp))
          (validation (map-get? validations { sector-id: validated-sector-id, participant: participant, validation-category: validated-validation-category }))
          (visibility (map-get? visibility-preferences { sector-id: validated-sector-id, participant: participant })))
      
      (if (is-none validation)
          (err u"Validation not found")
          (if (or (is-none visibility)
                  (get visible-validations (unwrap-panic visibility))
                  (can-access-private-data validated-sector-id participant tx-sender))
              (ok (unwrap-panic validation))
              (err u"Not authorized to view validation")
          )
      )
    )
  )
)

;; Check if a provider is authorized for verification
(define-read-only (is-service-authorized (sector-id uint) (service principal))
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
      (ok (is-authorized-validator validated-sector-id service))
    )
  )
)

;; Get privacy settings
(define-read-only (get-visibility-preferences (sector-id uint) (participant principal))
  (let ((validated-sector-id-resp (validate-sector-id sector-id)))
    (asserts! (is-ok validated-sector-id-resp) (err u"Invalid sector ID"))
    
    (let ((validated-sector-id (unwrap-panic validated-sector-id-resp)))
      (ok (default-to 
            {
              visible-score: true,
              visible-recommendations: true,
              visible-engagements: false,
              visible-validations: false,
              permitted-viewers: (list)
            }
            (map-get? visibility-preferences { sector-id: validated-sector-id, participant: participant })
          )
      )
    )
  )
)