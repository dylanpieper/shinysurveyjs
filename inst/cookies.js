function setCookie(name, value, days) {
   try {
       if (!name || typeof name !== "string") {
           throw new Error("Invalid cookie name");
       }
       const expires = new Date();
       expires.setTime(expires.getTime() + (days * 24 * 60 * 60 * 1000));
       const cookieValue = encodeURIComponent(JSON.stringify(value));
       document.cookie = `${name}=${cookieValue};expires=${expires.toUTCString()};path=/;SameSite=Strict`;
   } catch (error) {
       console.error("Error setting cookie:", error);
       Shiny.setInputValue("surveyError", {
           type: "CookieError",
           message: "Failed to set cookie",
           details: error.message
       });
   }
}

function getCookie(name) {
   try {
       if (!name || typeof name !== "string") {
           throw new Error("Invalid cookie name");
       }
       const nameEQ = name + "=";
       const cookies = document.cookie.split(";");
       for (const cookie of cookies) {
           let c = cookie.trim();
           if (c.indexOf(nameEQ) === 0) {
               const cookieValue = decodeURIComponent(c.substring(nameEQ.length));
               return JSON.parse(cookieValue);
           }
       }
       return null;
   } catch (error) {
       console.error("Error reading cookie:", error);
       return null;
   }
}

function deleteCookie(name) {
   try {
       if (!name || typeof name !== "string") {
           throw new Error("Invalid cookie name");
       }
       document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/`;
       document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;domain=${window.location.hostname}`;
       document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;domain=.${window.location.hostname}`;
   } catch (error) {
       console.error("Error deleting cookie:", error);
   }
}
