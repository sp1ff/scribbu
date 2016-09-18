#ifndef PROGRAM_SUPPORT_HH_INCLUDED
#define PROGRAM_SUPPORT_HH_INCLUDED

#include <string>
#include <vector>

namespace scribbu {

  namespace program {

    enum class text_tag_type {
      album, artist, content_type, encoded_by, title, year
    };

    enum class tag_type {
      album, artist, content_type, encoded_by, title, year
    };

    struct syntax_node_visitor
    {
    public:
      virtual ~syntax_node_visitor()
      { }
    };

    struct syntax_node {
      virtual void accept(syntax_node_visitor&) = 0;
    };

    class expression: public syntax_node
    { };

      class integer_expression: public expression
      { };

        class integer_literal: public integer_expression
        {
        public:
          integer_literal(long val):
            val_(val)
          { }
          virtual void accept(syntax_node_visitor&);
        private:
          long val_;
        };

      class text_expression: public expression
      { };

        class get_text_tag: public text_expression
        {
        public:
          get_text_tag(text_tag_type t):
            t_(t)
          { }
          virtual void accept(syntax_node_visitor&);
        private:
          text_tag_type t_;
        };

        // TODO: Work out ownership
        class static_text: public text_expression
        {
        public:
          static_text(std::string *p):
            p_(p)
          { }
          virtual void accept(syntax_node_visitor&);
        private:
          std::string *p_;
        };

    class term: public syntax_node
    { };

      // TODO: Work out ownership
      class equality_term: public term
      {
      public:
        enum class relation {
          equality, inequality, less_than, less_than_equal,
          greater_than, greater_than_equal
        };

      public:
        equality_term(expression *plhs,
                      expression *prhs,
                      relation    R = relation::equality):
          plhs_(plhs), prhs_(prhs), R_(R)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        expression *plhs_, *prhs_;
        relation R_;
      };

      class presence_term: public term
      {
      public:
        presence_term(tag_type tag, bool negate = false):
          tag_(tag), negate_(negate)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        tag_type tag_;
        bool negate_;
      };

      class textual_term: public term
      {
      public:
        enum class relation {
          contains
        };

      public:
        textual_term(text_expression *plhs,
                     text_expression *prhs,
                     relation         R):
          plhs_(plhs), prhs_(prhs), R_(R)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        text_expression *plhs_, *prhs_;
        relation R_;
      };

    class tag: public syntax_node
    { };

    class directive: public syntax_node
    { };

      class print: public directive
      {
      public:
        print()
        { }
        virtual void accept(syntax_node_visitor&)
        {
          // TODO: Write me!
        }
      };

      class set_tag: public directive
      { };

        // TODO: Work out ownership
        class set_text_tag: public set_tag
        {
        public:
          set_text_tag(text_tag_type tag, text_expression *pe):
            tag_(tag), pe_(pe)
          { }
          virtual void accept(syntax_node_visitor&);
        private:
          text_tag_type tag_;
          text_expression *pe_;
        };

    // TODO: Work out ownership
    class condition: public syntax_node
    { };

       class term_condition: public condition
       {
       public:
         term_condition(term *pterm):
           pterm_(pterm)
         { }
         virtual void accept(syntax_node_visitor&);
       private:
         term *pterm_;
       };

      // TODO: Work out ownership
      class and_condition: public condition
      {
      public:
        and_condition(condition *pc1, condition *pc2):
          pc1_(pc1), pc2_(pc2)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        condition *pc1_, *pc2_;
      };

      // TODO: Work out ownership
      class or_condition: public condition
      {
      public:
        or_condition(condition *pc1, condition *pc2):
          pc1_(pc1), pc2_(pc2)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        condition *pc1_, *pc2_;
      };

      class not_condition: public condition
      {
      public:
        not_condition(condition *pc):
          pc_(pc)
        { }
        virtual void accept(syntax_node_visitor&);
      private:
        condition *pc_;
      };

    // TODO: Work out ownership
    class conditional: public syntax_node
    {
    public:
      conditional(condition *pc, directive *pd1):
        pc_(pc), pd1_(pd1), pd2_(0)
      { }
      conditional(condition *pc, directive *pd1, directive *pd2):
        pc_(pc), pd1_(pd1), pd2_(pd2)
      { }
      virtual void accept(syntax_node_visitor&);
    private:
      condition *pc_;
      directive *pd1_;
      directive *pd2_;
    };

    // TODO: Work out ownership
    class statement: public syntax_node
    {
    public:
      statement(conditional *pc):
        pc_(pc), pd_(0)
      { }
      statement(directive *pd):
        pc_(0), pd_(pd_)
      { }
      virtual void accept(syntax_node_visitor&);
    private:
      conditional *pc_;
      directive *pd_;
    };

    struct start_symbol {
      bool fcondition_;
      scribbu::program::condition *pcondition_;
      std::vector<scribbu::program::statement*> *pstatement_list_;
    };


///////////////////////////////////////////////////////////////////////////////
    class integer_literal_visitor: public syntax_node_visitor
    {
    public:
      void visit(integer_literal&)
      {
        // TODO: Implement me!
      }
    };

    class get_text_tag_visitor: public syntax_node_visitor
    {
    public:
      void visit(get_text_tag &)
      {
        // TODO: Implement me!
      }
    };

    class static_text_visitor: public syntax_node_visitor
    {
    public:
      void visit(static_text &)
      {
        // TODO: Implement me!
      }
    };

    class equality_term_visitor: public syntax_node_visitor
    {
    public:
      void visit(equality_term &)
      {
        // TODO: Implement me!
      }
    };

    class presence_term_visitor: public syntax_node_visitor
    {
    public:
      void visit(presence_term &)
      {
        // TODO: Implement me!
      }
    };

    class textual_term_visitor: public syntax_node_visitor
    {
    public:
      void visit(textual_term &)
      {
        // TODO: Implement me!
      }
    };

    class set_text_tag_visitor: public syntax_node_visitor
    {
    public:
      void visit(set_text_tag &)
      {
        // TODO: Implement me!
      }
    };

    class term_condition_visitor: public syntax_node_visitor
    {
    public:
      void visit(term_condition &)
      {
        // TODO: Implement me!
      }
    };

    class and_condition_visitor: public syntax_node_visitor
    {
    public:
      void visit(and_condition &)
      {
        // TODO: Implement me!
      }
    };

    class or_condition_visitor: public syntax_node_visitor
    {
    public:
      void visit(or_condition &)
      {
        // TODO: Implement me!
      }
    };

    class not_condition_visitor: public syntax_node_visitor
    {
    public:
      void visit(not_condition &)
      {
        // TODO: Implement me!
      }
    };

    class conditional_visitor: public syntax_node_visitor
    {
    public:
      void visit(conditional &)
      {
        // TODO: Implement me!
      }
    };

    class statement_visitor: public syntax_node_visitor
    {
    public:
      void visit(statement &)
      {
        // TODO: Implement me!
      }
    };

  } // End namespace program.

} // End namespace scribbu.

#endif // not PROGRAM_SUPPORT_HH_INCLUDED
