package ucesoft.smd.cpu.svp

/**
 * @author Alessandro Abbruzzetti
 *         Created on 10/05/2024 19:42  
 */
enum RegisterType:
  case
  // general registers
  /*00*/BLIND,
  /*01*/X,
  /*02*/Y,
  /*03*/A,
  /*04*/ST,
  /*05*/STACK,
  /*06*/PC,
  /*07*/P,
  // external registers
  /*08*/PM0,
  /*09*/PM1,
  /*0A*/PM2,
  /*0B*/XST,
  /*0C*/PM4,
  /*0D*/EXT5,
  /*0E*/PMC,
  /*0F*/AL,
  // pointer registers
  /*10*/R0,
  /*11*/R1,
  /*12*/R2,
  /*13*/R3,
  /*14*/R4,
  /*15*/R5,
  /*16*/R6,
  /*17*/R7
  