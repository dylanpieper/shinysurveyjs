// Core variables with proper typing
/** @type {Survey.Model} */
let survey = null;
/** @type {Object.<string, any>} */
let storedParams = {};
let isInitializing = false;
let initializationTimer = null;

// Constants
const COOKIE_NAME = "surveyProgress";
const COOKIE_EXPIRATION_DAYS = %d;
const DEBOUNCE_DELAY = 100;
const CONDITION_DELAY = 200;

// Type definitions and utility functions
/** @typedef {Object.<string, any>} SurveyData */
/** @typedef {Object} ParamValue
* @property {*} value - The value to set
* @property {string} text - The display text
*/
