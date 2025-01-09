Shiny.addCustomMessageHandler("loadSurvey", function(data) {
   if (isInitializing) {
       console.log("Survey initialization already in progress, skipping");
       return;
   }

   if (initializationTimer) {
       clearTimeout(initializationTimer);
   }

   isInitializing = true;
   initializationTimer = setTimeout(() => {
       try {
           initializeSurvey(data);
       } finally {
           isInitializing = false;
       }
   }, DEBOUNCE_DELAY);
});
