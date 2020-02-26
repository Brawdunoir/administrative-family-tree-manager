with Arbre_Genealogique; 
with Registre;
with Ada.Text_IO;
use  Ada.Text_IO;
use Arbre_Genealogique;
use Registre;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;


procedure Test_Arbre_Genealogique is



    procedure Tester_Initialiser(arbre1 : in out T_Arbre_Gen; reg : in out T_Registre) is 
        CleYL : Integer;
    begin
        -- On initialise l'arbre.
        Initialiser_Gen (arbre1,reg, To_Unbounded_String("Lacroix"), To_Unbounded_String("Yann"), 1, 3, 1999, 'H');

        Afficher_Arbre_Genealogique (arbre1);
        New_Line;

        -- La cle de Yann est 310199900
        CleYL := 310199900;
        
        -- On vérifie le nombre de fils.
        pragma Assert (Nombre_Ancetre (arbre1, CleYL) = 1);
        pragma Assert (not Est_Vide_Gen (arbre1));
            
        Vider(arbre1,reg);
        pragma Assert (Est_Vide_Gen (arbre1));
	

    end Tester_Initialiser;



    procedure Tester_Ajouter(arbre1 : in out T_Arbre_Gen; reg : in out T_Registre) is 

        
        CleDJ : Integer;
	CleDM : Integer;

    begin
        -- On initialise l'arbre.
	
        Initialiser_Gen (arbre1,reg, To_Unbounded_String("Dupont"), To_Unbounded_String("Jean"), 12, 3, 1999, 'H');
	
	    CleDJ := 312199900;
	
        -- On ajoute une mere à Jean.
        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Marie"), 21, 6, 1967, 'F');
	
	    CleDM := 621196700;

        Afficher_Arbre_Genealogique (arbre1);
        New_Line;

        pragma Assert (not Est_Vide_Gen (arbre1));
        pragma Assert (Nombre_Ancetre (arbre1, CleDJ) = 2);

        pragma Assert (Nombre_Ancetre (arbre1, CleDM) = 1);
            
        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Louis"), 18, 9, 1964, 'H' );

        Afficher_Arbre_Genealogique (arbre1);
        New_Line;

        -- On vérifie que l'arbre n'est plus vide, et que le
        -- nombre de fils est de 2.
        pragma Assert (not Est_Vide_Gen (arbre1));
        pragma Assert (Nombre_Ancetre (arbre1, CleDJ) = 3);

        
  
        -- On ajoute deux parent à Marie.
        Ajouter_Parent (arbre1, reg,CleDM, To_Unbounded_String("Girard"), To_Unbounded_String("Andre"), 8, 11, 1931, 'H' );
        Ajouter_Parent (arbre1, reg,CleDM, To_Unbounded_String("Girard"), To_Unbounded_String("Madeleine"), 7, 7, 1935, 'F' );
    

        Afficher_Arbre_Genealogique (arbre1);
        New_Line;

        -- On vérifie qu'il a bien le bon nombre de fils selon
        -- le noeud qu'on sélectionne
        pragma Assert (Nombre_Ancetre (arbre1, CleDM) = 3);
        pragma Assert (Nombre_Ancetre (arbre1, CleDJ) = 5);

        

        -- On vérifie que le programme lève une exception si la Cle_Parent est Absente.

        --Ajouter_Parent (arbre1,reg, ,871956, "Girard", "Madeleine", 7, 8, 1935, 'F' );

        -- On vérifie que le programme lève une exception si le noeud a déja un fils Droit.
        --Ajouter_Fils_Droit (arbre1,reg,CleDM, "Girard", "Madeleine", 7, 7, 1935, 'F' );



        Vider(arbre1,reg);
        
    end Tester_Ajouter;

    procedure Tester_Supprimer(arbre1 : in out T_Arbre_Gen; reg : in out T_Registre) is 

	CleDJ : Integer;
	CleDM : Integer;

    begin
	
        Initialiser_Gen (arbre1,reg, To_Unbounded_String("Dupont"), To_Unbounded_String("Jean"), 12, 3, 1999, 'H');
	
	CleDJ := 312199900;
        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Marie"), 21, 6, 1967, 'F');
	
	CleDM := 621196700;

        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Louis"), 18, 9, 1964, 'H' );

        Ajouter_Parent (arbre1, reg,CleDM, To_Unbounded_String("Girard"), To_Unbounded_String("Andre"), 8, 11, 1931, 'H' );
        Ajouter_Parent (arbre1, reg,CleDM, To_Unbounded_String("Girard"), To_Unbounded_String("Madeleine"), 7, 7, 1935, 'F' );
   
        Afficher_Arbre_Genealogique (arbre1);
        New_Line;
        
        Supprimer (arbre1, reg, CleDM);
        Afficher_Arbre_Genealogique (arbre1);
	    New_Line;
	
    
        -- On s'assure que l'arbre n'est pas complétement supprimé.
        pragma Assert (not Est_Vide_Gen (arbre1));
        -- On s'assure que Louis n'a pas été touché.
        pragma Assert (Nombre_Ancetre (arbre1, CleDJ) = 2);
     
        
        Supprimer(arbre1, reg, CleDJ);
        pragma Assert (Est_Vide_Gen (arbre1));
        -- On vide l'arbre.
        Vider (arbre1, reg);
    end Tester_Supprimer;



    procedure Tester_Informations(arbre1 : in out T_Arbre_Gen; reg : in out T_Registre) is 

        CleDJ : Integer;
	    CleDM : Integer;
	    CleDL: Integer;

    begin
	
        Initialiser_Gen (arbre1,reg, To_Unbounded_String("Dupont"), To_Unbounded_String("Jean"), 12, 3, 1999, 'H');
	
	    CleDJ := 312199900;
        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Marie"), 21, 6, 1967, 'F');
	
	    CleDM := 621196700;

        Ajouter_Parent (arbre1, reg, CleDJ, To_Unbounded_String("Dupont"), To_Unbounded_String("Louis"), 18, 9, 1964, 'H' );

	    CleDL := 918196400;

        Ajouter_Parent (arbre1, reg,CleDM, To_Unbounded_String("Girard"), To_Unbounded_String("Andre"), 8, 11, 1931, 'H' );
        
           Afficher_Arbre_Genealogique (arbre1);
        New_Line;
	
        --Doit afficher 312199900.
        Afficher_Biparental (arbre1);
        --Doit afficher 621196700.
        Afficher_Monoparental(arbre1);
        --Doit afficher 918196400 et 1108193100
        Afficher_Orphelins(arbre1);
        -- Doit afficher 621196700 et 918196400
        Ancetre_Generation_X(arbre1, CleDJ, 1);

        --Doit afficher 1108193100
        Ancetres_Sur_N_Generations(arbre1, CleDM, 1);
        
        Ajouter_Parent (arbre1, reg, CleDL, To_Unbounded_String("Girard"), To_Unbounded_String("Andre"), 1, 1, 1933, 'H' );
        --Doit afficher Dupont
        Ancetres_Homonymes(arbre1, reg, CleDL, CleDM);

        Vider (arbre1, reg);
    end Tester_Informations;

    arbre1 : T_arbre_Gen;
    reg : T_Registre;

begin

    Tester_Initialiser(arbre1,reg);
    Tester_Ajouter(arbre1,reg);
    Tester_Supprimer(arbre1,reg);
    Tester_Informations(arbre1,reg);
    Put("OK");

end Test_Arbre_Genealogique;
