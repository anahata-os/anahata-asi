- **Files **: Use **Round Brackets** `()`.
  - name annotation: `(displayName)` if in **exactly one** session (e.g., `(messi)`).
  - name annotation : `(n)` if in **multiple** sessions (e.g., `(2)`).  
  - icon: anahata icon badge at **16,0**
  - tooltip: (anahata logo) In context in: 
            session 1 `displayName`,'\n'
            session 2 displayName

- **Folders**:  Use **Square Brackets** `[]`.
  - name annotations: `[sum1][sum2]...` (one bracket group[] per active session) with the recursive acumulative number of resources in context.
  - icon: anahata icon badge at **16,0**
  - tooltip: (anahata logo) In Context in: 
            session 1 `displayName`: 3 resources
            session 2 displayName: 2 resources

  
- **Source Packages**: Use **Square Brackets** `[]`.
  - format: `[sum1][sum2]...` (one bracket per active session with the sum of all resources in that package **only**, not the recursive totals like in folders).  
  - icon: anahata icon badge at **16,0**
  - tooltip: same as folders but just for that package

- **Project** 
  - name annotation: `[sum1][sum]` The total number of "effectively providing" context providers regardless of their nature (resources/toolkits/standalone context providers)    
  - anahata badge just like folders or files
  - tooltip: 
    anahata logo: In context in: 
    (for each "active" session:)
     <b>session1DisplayName<b>: 
            Providers: Comma separated List of 'effectively providing' context providers for that session


